#define LUMIX_NO_CUSTOM_CRT
#include "editor/asset_browser.h"
#include "editor/settings.h"
#include "editor/studio_app.h"
#include "editor/utils.h"
#include "editor/world_editor.h"
#include "engine/allocator.h"
#include "engine/array.h"
#include "engine/crt.h"
#include "engine/log.h"
#include "engine/os.h"
#include "engine/string.h"
#include "engine/universe.h"
#include "renderer/material.h"
#include "renderer/render_scene.h"
#include <math.h>

#include "imgui/imgui.h"


namespace Lumix {

struct ProceduralGeomPlugin;

namespace {

enum { OUTPUT_FLAG = 1 << 31 };

enum class NodeType : u32 { 
	OUTPUT,
	CUBE,
	DISTRIBUTE_POINT_ON_FACES,
	GRID,
	TRANSFORM,
	MERGE,
	SPHERE,
	CONE
};

struct Geometry {
	struct Vertex {
		Vec3 position;
		Vec2 uv;
		Vec3 normal;
		Vec3 tangent;
	};

	Geometry(IAllocator& allocator)
		: vertices(allocator)
		, indices(allocator)
	{}

	Geometry&& move() { return static_cast<Geometry&&>(*this); }

	Array<Vertex> vertices;
	Array<u32> indices;
	gpu::PrimitiveType type;
};

struct Node {
	virtual void gui() = 0;

	virtual NodeType getType() = 0;

	virtual bool getGeometry(u16 output_idx, Geometry* result) = 0;
	virtual void serialize(OutputMemoryStream& blob) {}
	virtual void deserialize(InputMemoryStream& blob) {}

	void nodeGUI() {
		ImGuiEx::BeginNode(m_id, m_pos, &m_selected);
		m_input_counter = 0;
		m_output_counter = 0;
		gui();
		ImGuiEx::EndNode();
	}

	void inputSlot() {
		ImGuiEx::Pin(m_id | ((u32)m_input_counter << 16), true);
		++m_input_counter;
	}

	void outputSlot() {
		ImGuiEx::Pin(m_id | ((u32)m_output_counter << 16) | OUTPUT_FLAG, false);
		++m_output_counter;
	}

	ProceduralGeomPlugin* m_plugin;
	IAllocator* m_allocator;
	u16 m_id;
	bool m_selected = false;
	ImVec2 m_pos = ImVec2(0, 0);
	u16 m_input_counter = 0;
	u16 m_output_counter = 0;
};

struct Link {
	u32 from;
	u32 to;
};

struct Header {
	static constexpr u32 MAGIC = '_PGM';
	u32 magic = MAGIC;
	u32 version = 0;
};

} // anonymous namespace

static const ComponentType MODEL_INSTANCE_TYPE = reflection::getComponentType("model_instance");

struct ProceduralGeomPlugin : StudioApp::GUIPlugin {
	ProceduralGeomPlugin(StudioApp& app)
		: m_app(app)
		, m_allocator(app.getAllocator())
		, m_nodes(app.getAllocator())
		, m_links(app.getAllocator())
		, m_recent_paths(app.getAllocator())
	{
		Node* n = addNode(NodeType::OUTPUT);
		n->m_pos = ImVec2(100, 100);

		m_toggle_ui.init("Procedural editor", "Toggle procedural editor", "procedural_editor", "", true);
		m_toggle_ui.func.bind<&ProceduralGeomPlugin::toggleOpen>(this);
		m_toggle_ui.is_selected.bind<&ProceduralGeomPlugin::isOpen>(this);
		app.addWindowAction(&m_toggle_ui);
	}

	~ProceduralGeomPlugin(){
		m_app.removeAction(&m_toggle_ui);
	}

	bool isOpen() const { return m_is_open; }
	void toggleOpen() { m_is_open = !m_is_open; }
	bool hasFocus() override { return m_has_focus; }

	void load(const char* path) {
		os::InputFile file;
		if (!file.open(path)) {
			logError("Failed to load ", path);
			return;
		}

		const u32 data_size = (u32)file.size();
		OutputMemoryStream data(m_allocator);
		data.resize(data_size);
		if (!file.read(data.getMutableData(), data_size)) {
			logError("Failed to load shader ", path);
			file.close();
			return;
		}
		file.close();

		InputMemoryStream blob(data);
		deserialize(blob, path);

		pushRecent(path);
	}

	void pushRecent(const char* path) {
		String p(path, m_app.getAllocator());
		m_recent_paths.eraseItems([&](const String& s) { return s == path; });
		m_recent_paths.push(static_cast<String&&>(p));
	}

	void load() {
		ASSERT(false);
	}

	void clear() {
		m_nodes.clear();
		m_node_id_genereator = 1;
		m_links.clear();
	}
	
	void serialize(OutputMemoryStream& blob) {
		Header header;
		blob.write(header);
		blob.writeString(m_material);
		blob.write(m_nodes.size());
		for (UniquePtr<Node>& node : m_nodes) {
			blob.write(node->getType());
			blob.write(node->m_id);
			blob.write(node->m_pos);
			node->serialize(blob);
		}
		blob.write(m_links.size());
		blob.write(m_links.begin(), m_links.byte_size());
	}

	void deserialize(InputMemoryStream& blob, const char* path) {
		clear();

		Header header;
		blob.read(header);
		m_material = blob.readString();
		if (header.magic != Header::MAGIC) {
			logError("Corrupted file ", path);
			return;
		}
		u32 count;
		blob.read(count);
		m_nodes.reserve(count);
		for (u32 i = 0; i < count; ++i) {
			NodeType type;
			blob.read(type);
			Node* node = addNode(type);
			blob.read(node->m_id);
			blob.read(node->m_pos);
			node->deserialize(blob);
		}
		blob.read(count);
		m_links.resize(count);
		blob.read(m_links.begin(), m_links.byte_size());
	}

	void save() {
		char path[LUMIX_MAX_PATH];
		
		if (!os::getSaveFilename(Span(path), "Procedural geometry\0*.pgm", "pgm")) return;
		OutputMemoryStream blob(m_allocator);
		serialize(blob);
		
		os::OutputFile file;
		if(!file.open(path)) {
			logError("Could not save ", path);
			return;
		}

		bool success = file.write(blob.data(), blob.size());
		file.close();
		if (!success) {
			logError("Could not save ", path);
		}

		pushRecent(path);
	}

	void onSettingsLoaded() override {
		Settings& settings = m_app.getSettings();
		m_is_open = settings.getValue(Settings::GLOBAL, "is_procedural_geom_editor_open", false);
		char tmp[LUMIX_MAX_PATH];
		m_recent_paths.clear();
		for (u32 i = 0; ; ++i) {
			const StaticString<32> key("procedural_geom_editor_recent_", i);
			const u32 len = settings.getValue(Settings::LOCAL, key, Span(tmp));
			if (len == 0) break;
			m_recent_paths.emplace(tmp, m_app.getAllocator());
		}
	}

	void onBeforeSettingsSaved() override {
		Settings& settings = m_app.getSettings();
		settings.setValue(Settings::GLOBAL, "is_procedural_geom_editor_open", m_is_open);
		for (const String& p : m_recent_paths) {
			const u32 i = u32(&p - m_recent_paths.begin());	
			const StaticString<32> key("procedural_geom_editor_recent_", i);
			settings.setValue(Settings::LOCAL, key, p.c_str());
		}
	}

	void onWindowGUI() override {
		m_has_focus = false;
		if (!m_is_open) return;

		ImGui::SetNextWindowSize(ImVec2(200, 200), ImGuiCond_FirstUseEver);
		if (ImGui::Begin("Procedural geometry", &m_is_open, ImGuiWindowFlags_MenuBar)) {
			m_has_focus = ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows);		
			if (ImGui::BeginMenuBar()) {
				if (ImGui::BeginMenu("File")) {
					if (ImGui::MenuItem("Load")) load();
					if (ImGui::MenuItem("Save")) save();
					if (ImGui::BeginMenu("Recet", !m_recent_paths.empty())) {
						for (const String& path : m_recent_paths) {
							if (ImGui::MenuItem(path.c_str())) load(path.c_str());
						}
						ImGui::EndMenu();
					}
					ImGui::EndMenu();
				}
				if (ImGui::MenuItem("Apply", nullptr, false, !m_material.empty())) apply();
				ImGui::EndMenuBar();
			}

			m_app.getAssetBrowser().resourceInput("material", Span(m_material.data), Material::TYPE);

			m_canvas.begin();
			ImGuiEx::BeginNodeEditor("pg", &m_offset);
			for (UniquePtr<Node>& node : m_nodes) {
				node->nodeGUI();
			}

			i32 hovered_link = -1;
			for (i32 i = 0, c = m_links.size(); i < c; ++i) {
				const Link& link = m_links[i];
				ImGuiEx::NodeLink(link.from | OUTPUT_FLAG, link.to);
				if (ImGuiEx::IsLinkHovered()) {
					hovered_link = i;
				}
			}

			u32 newfrom, newto;
			if (ImGuiEx::GetNewLink(&newfrom, &newto)) {
				m_links.eraseItems([&](const Link& link){ return link.to == newto; });
				m_links.emplace(Link{newfrom, newto});
			}

			ImGuiEx::EndNodeEditor();

			if (ImGui::IsItemHovered() && ImGui::IsMouseClicked(0)) {
				if (ImGui::GetIO().KeyAlt && hovered_link != -1) {
					m_links.erase(hovered_link);
				}
			}

			if (ImGui::IsItemHovered() && ImGui::IsMouseClicked(1)) {
				m_context_pos = ImGui::GetMousePos() - m_offset;
				ImGui::OpenPopup("context_menu");
			}

			if(ImGui::BeginPopup("context_menu")) {
				static char filter[64] = "";
				
				Node* new_node = nullptr;
				if (ImGui::MenuItem("Transform")) new_node = addNode(NodeType::TRANSFORM);
				if (ImGui::MenuItem("Cone")) new_node = addNode(NodeType::CONE);
				if (ImGui::MenuItem("Cube")) new_node = addNode(NodeType::CUBE);
				if (ImGui::MenuItem("Grid")) new_node = addNode(NodeType::GRID);
				if (ImGui::MenuItem("Sphere")) new_node = addNode(NodeType::SPHERE);
				if (ImGui::MenuItem("Merge")) new_node = addNode(NodeType::MERGE);

				if (new_node) new_node->m_pos = m_context_pos;

				ImGui::EndPopup();
			}		

			m_canvas.end();
		}
		ImGui::End();
	}
	
	void apply();
	Node* addNode(NodeType type);

	const char* getName() const override { return "procedural_geom"; }

	u16 m_node_id_genereator = 1;
	IAllocator& m_allocator;
	Array<UniquePtr<Node>> m_nodes;
	Array<Link> m_links;
	StudioApp& m_app;
	ImVec2 m_offset = ImVec2(0, 0);
	ImGuiEx::Canvas m_canvas;
	bool m_is_open = true;
	StaticString<LUMIX_MAX_PATH> m_material;
	Array<String> m_recent_paths;
	ImVec2 m_context_pos;
	bool m_has_focus = false;
	Action m_toggle_ui;
};

static u16 toNodeId(int id) {
	return u16(id);
}

static u16 toAttrIdx(int id) {
	return u16(u32(id) >> 16);
}

template <typename F>
static void	forEachInput(const ProceduralGeomPlugin& editor, int node_id, const F& f) {
	for (const Link& link : editor.m_links) {
		if (toNodeId(link.to) == node_id) {
			const int iter = editor.m_nodes.find([&](const UniquePtr<Node>& node) { return node->m_id == toNodeId(link.from); }); 
			Node* from = editor.m_nodes[iter].get();
			const u16 from_attr = toAttrIdx(link.from);
			const u16 to_attr = toAttrIdx(link.to);
			f(from, from_attr, to_attr, u32(&link - editor.m_links.begin()));
		}
	}
}

struct Input {
	Node* node = nullptr;
	u16 output_idx;
	bool getGeometry(Geometry* result) const { return node->getGeometry(output_idx, result); }
	operator bool() const { return node != nullptr; }
};

static Input getInput(const ProceduralGeomPlugin& editor, u16 node_id, u16 input_idx) {
	Input res;
	forEachInput(editor, node_id, [&](Node* from, u16 from_attr, u16 to_attr, u32 link_idx){
		if (to_attr == input_idx) {
			res.output_idx = from_attr;
			res.node = from;
		}
	});
	return res;
}

struct CylinderNode : Node {
};

struct ConeNode : Node {
	NodeType getType() override { return NodeType::CONE; }
	
	void serialize(OutputMemoryStream& blob) override {
		blob.write(subdivision);
		blob.write(base_size);
		blob.write(height);
	}

	void deserialize(InputMemoryStream& blob) override {
		blob.read(subdivision);
		blob.read(base_size);
		blob.read(height);
	}
	
	bool getGeometry(u16 output_idx, Geometry* result) override {
		result->vertices.reserve(2 * subdivision + 2);
		result->indices.reserve(subdivision * 6);

		for (u32 i = 0; i < subdivision; ++i) {
			const float angle = i / float(subdivision) * 2 * PI;
			Geometry::Vertex& vertex = result->vertices.emplace();
			vertex.position = Vec3(cosf(angle), sinf(angle), 0) * base_size;
			vertex.uv = Vec2((float)i, 0);
			vertex.normal = normalize(vertex.position);
			vertex.tangent = Vec3(vertex.normal.y, -vertex.normal.x, 0);
		}

		{
			Geometry::Vertex& vertex = result->vertices.emplace();
			vertex.position = Vec3(0, 0, height);
			vertex.normal = Vec3(0, 0, 1);
			vertex.tangent = Vec3(1, 0, 0);
			vertex.uv = Vec2(0, 1);
		}

		for (u32 i = 0; i < subdivision; ++i) {
			const float angle = i / float(subdivision) * 2 * PI;
			Geometry::Vertex& vertex = result->vertices.emplace();
			vertex.position = Vec3(cosf(angle), sinf(angle), 0) * base_size;
			vertex.uv = Vec2((float)i, 0);
			vertex.normal = Vec3(0, 0, -1);
			vertex.tangent = Vec3(1, 0, 0);
		}

		{
			Geometry::Vertex& vertex = result->vertices.emplace();
			vertex.position = Vec3(0, 0, 0);
			vertex.normal = Vec3(0, 0, -1);
			vertex.tangent = Vec3(1, 0, 0);
			vertex.uv = Vec2(0, 0);
		}

		for (u32 i = 0; i < subdivision; ++i) {
			result->indices.push(i);
			result->indices.push((i + 1) % subdivision);
			result->indices.push(subdivision);
		}

		for (u32 i = 0; i < subdivision; ++i) {
			result->indices.push(subdivision + 1 + i);
			result->indices.push(subdivision + 1 + subdivision);
			result->indices.push(subdivision + 1 + (i + 1) % subdivision);
		}

		result->type = gpu::PrimitiveType::TRIANGLES;
		return true;
	}

	void gui() override {
		ImGuiEx::NodeTitle("Cone");
		outputSlot();
		ImGui::DragInt("Subdivision", (i32*)&subdivision);
		ImGui::DragFloat("Base size", &base_size);
		ImGui::DragFloat("Height", &height);
	}	

	u32 subdivision = 30;
	float base_size = 1;
	float height = 3;
};

struct SphereNode : Node {
	NodeType getType() override { return NodeType::SPHERE; }
	void serialize(OutputMemoryStream& blob) override {
		blob.write(size);
	}

	void deserialize(InputMemoryStream& blob) override {
		blob.read(size);
	}
	
	bool getGeometry(u16 output_idx, Geometry* result) override {
		result->vertices.reserve(6 * size * size);
		result->indices.reserve(12 * (size - 1));

		auto push_side = [result, this](u32 coord0, u32 coord1, u32 coord2, float coord2_val){
			const u32 offset = result->vertices.size();
			for (u32 j = 0; j < size; ++j) {
				for (u32 i = 0; i < size; ++i) {
					Geometry::Vertex& v = result->vertices.emplace();
					v.position[coord0] = i / float(size - 1) * 2 - 1;
					v.position[coord1] = j / float(size - 1) * 2 - 1;
					v.position[coord2] = coord2_val;
					v.position = normalize(v.position);
					// TODO proper UV and tangent
					v.uv = { i / float(size - 1), j / float(size - 1) };
					v.normal = v.position;
					v.tangent = normalize(Vec3(0, v.position.z, v.position.y));
				}
			}
			for (u32 j = 0; j < size - 1; ++j) {
				for (u32 i = 0; i < size - 1; ++i) {
					result->indices.push(offset + i + j * size);
					result->indices.push(offset + i + 1 + j * size);
					result->indices.push(offset + i + (j + 1) * size);

					result->indices.push(offset + i + (j + 1) * size);
					result->indices.push(offset + i + 1 + j * size);
					result->indices.push(offset + i + 1 + (j + 1) * size);
				}
			}
		};

		push_side(0, 1, 2, 1);
		push_side(1, 0, 2, -1);

		push_side(2, 0, 1, 1);
		push_side(0, 2, 1, -1);
		
		push_side(1, 2, 0, 1);
		push_side(2, 1, 0, -1);
		result->type = gpu::PrimitiveType::TRIANGLES;

		return true;
	}

	void gui() override {
		ImGuiEx::NodeTitle("Sphere");
		outputSlot();
		ImGui::DragInt("Size", (i32*)&size);
	}

	u32 size = 10;
};

struct CubeNode : Node {
	NodeType getType() override { return NodeType::CUBE; }

	void serialize(OutputMemoryStream& blob) override {
		blob.write(size);
	}

	void deserialize(InputMemoryStream& blob) override {
		blob.read(size);
	}

	bool getGeometry(u16 output_idx, Geometry* result) override {
		result->vertices.reserve(2 * size.x * size.y + 2 * size.y * size.z + 2 * size.x * size.z);
		const IVec3 s1 = size + IVec3(-1);
		result->indices.reserve(4 * s1.x * s1.y + 4 * s1.y * s1.z + 4 * s1.x * s1.z);

		auto push_side = [result](u32 size_x, u32 size_y, u32 coord0, u32 coord1, u32 coord2, float coord2_val){
			const u32 offset = result->vertices.size();
			for (u32 j = 0; j < size_y; ++j) {
				for (u32 i = 0; i < size_x; ++i) {
					Geometry::Vertex& v = result->vertices.emplace();
					v.position[coord0] = i / float(size_x - 1) * 2 - 1;
					v.position[coord1] = j / float(size_y - 1) * 2 - 1;
					v.position[coord2] = coord2_val;
					v.uv = { i / float(size_x - 1), j / float(size_y - 1) };
					v.normal[coord0] = 0;
					v.normal[coord1] = 0;
					v.normal[coord2] = coord2_val;
					v.tangent[coord0] = coord2_val;
					v.tangent[coord1] = 0;
					v.tangent[coord2] = 0;
				}
			}
			for (u32 j = 0; j < size_y - 1; ++j) {
				for (u32 i = 0; i < size_x - 1; ++i) {
					result->indices.push(offset + i + j * size_x);
					result->indices.push(offset + i + 1 + j * size_x);
					result->indices.push(offset + i + (j + 1) * size_x);

					result->indices.push(offset + i + (j + 1) * size_x);
					result->indices.push(offset + i + 1 + j * size_x);
					result->indices.push(offset + i + 1 + (j + 1) * size_x);
				}
			}
		};

		push_side(size.x, size.y, 0, 1, 2, 1);
		push_side(size.x, size.y, 1, 0, 2, -1);

		push_side(size.x, size.z, 2, 0, 1, 1);
		push_side(size.x, size.z, 0, 2, 1, -1);
		
		push_side(size.y, size.z, 1, 2, 0, 1);
		push_side(size.y, size.z, 2, 1, 0, -1);
		result->type = gpu::PrimitiveType::TRIANGLES;

		return true;
	}

	void gui() override {
		ImGuiEx::NodeTitle("Cube");
		outputSlot();
		ImGui::DragInt("Vertices X", &size.x);
		ImGui::DragInt("Vertices Y", &size.y);
		ImGui::DragInt("Vertices Z", &size.z);
	}

	IVec3 size = IVec3(2);	
};

struct MergeNode : Node {
	NodeType getType() override { return NodeType::MERGE; }

	bool getGeometry(u16 output_idx, Geometry* result) override {
		const Input input0 = getInput(*m_plugin, m_id, 0);
		if (!input0) return false;
		if (!input0.getGeometry(result)) return false;

		const Input input1 = getInput(*m_plugin, m_id, 1);
		if (input1) {
			Geometry tmp(*m_allocator);
			if (!input1.getGeometry(&tmp)) return false;
			if (result->type != tmp.type) return false;
			
			const u32 old_size = result->vertices.size();

			result->vertices.resize(result->vertices.size() + tmp.vertices.size());
			memcpy(&result->vertices[old_size], tmp.vertices.begin(), tmp.vertices.byte_size());

			result->indices.reserve(result->indices.size() + tmp.indices.size());
			for (u32 idx : tmp.indices) {
				result->indices.push(idx + old_size);
			}
		}

		return true;
	}

	void gui() override {
		ImGuiEx::NodeTitle("Merge");
		ImGui::BeginGroup();
		inputSlot(); ImGui::TextUnformatted("A");
		inputSlot(); ImGui::TextUnformatted("B");
		ImGui::EndGroup();
		ImGui::SameLine();		
		outputSlot();
	}
};

struct TransformNode : Node {
	NodeType getType() override { return NodeType::TRANSFORM; }

	void serialize(OutputMemoryStream& blob) {
		blob.write(translation);
		blob.write(euler);
		blob.write(scale);
	}
	
	void deserialize(InputMemoryStream& blob) {
		blob.read(translation);
		blob.read(euler);
		blob.read(scale);
	}

	bool getGeometry(u16 output_idx, Geometry* result) override {
		const Input input = getInput(*m_plugin, m_id, 0);
		if (!input) return false;
		if (!input.getGeometry(result)) return false;

		Quat rotation;
		rotation.fromEuler(euler);

		// TODO transform normal and tangent
		for (Geometry::Vertex& v : result->vertices) {
			v.position = rotation.rotate(v.position) * scale + translation;
		}

		return true;
	}

	void gui() override {
		ImGuiEx::NodeTitle("Transform");
		inputSlot();
		ImGui::BeginGroup();
		ImGui::DragFloat3("Translation", &translation.x);
		ImGuiEx::InputRotation("Rotation", &euler.x);
		ImGui::DragFloat3("Scale", &scale.x);
		ImGui::EndGroup();
		ImGui::SameLine();
		outputSlot();
	}

	Vec3 translation = Vec3::ZERO;
	Vec3 euler = Vec3::ZERO;
	Vec3 scale = Vec3(1);
};

struct GridNode : Node {
	NodeType getType() override { return NodeType::GRID; }

	bool getGeometry(u16 output_idx, Geometry* result) override {
		result->type = gpu::PrimitiveType::TRIANGLES;
		result->vertices.resize((m_rows + 1) * (m_cols + 1));
		for (i32 j = 0; j < m_rows + 1; ++j) {
			for (i32 i = 0; i < m_cols + 1; ++i) {
				result->vertices[i + j * (m_cols + 1)] = {
					{ i / float(m_cols) * 2 - 1, j / float(m_rows) * 2 - 1, 0 },
					{ i / float(m_cols), j / float(m_rows) },
					{0, 0, 1},
					{1, 0, 0}
				};
			}
		}

		result->indices.reserve(m_rows * m_cols * 6);
		for (i32 j = 0; j < m_rows; ++j) {
			for (i32 i = 0; i < m_cols; ++i) {
				result->indices.push(i + j * (m_cols + 1));
				result->indices.push(i + 1 + j * (m_cols + 1));
				result->indices.push(i + (j + 1) * (m_cols + 1));

				result->indices.push(i + (j + 1) * (m_cols + 1));
				result->indices.push(i + 1 + j * (m_cols + 1));
				result->indices.push(i + 1 + (j + 1) * (m_cols + 1));
			}
		}

		return true;
	}

	void gui() override {
		ImGuiEx::NodeTitle("Grid");
		outputSlot();
		ImGui::DragInt("Columns", &m_cols);
		ImGui::DragInt("Rows", &m_rows);
	}

	i32 m_cols = 10;
	i32 m_rows = 10;
};

struct DistributePointsOnFacesNode : Node {
	NodeType getType() override { return NodeType::DISTRIBUTE_POINT_ON_FACES; }

	bool getGeometry(u16 output_idx, Geometry* result) override {
		// TODO
		return false;
	}

	void gui() override {
		ImGuiEx::NodeTitle("Distribute points on faces");
		inputSlot();
		ImGui::TextUnformatted(" ");
		ImGui::SameLine();
		outputSlot();
	}
};

struct OutputGeometryNode : Node {
	NodeType getType() override { return NodeType::OUTPUT; }
	
	bool getGeometry(u16 output_idx, Geometry* result) override {
		const Input input = getInput(*m_plugin, m_id, 0);
		if (input) return input.getGeometry(result);
		return false;
	}

	void gui() override {
		ImGuiEx::NodeTitle("Output");
		inputSlot();
		ImGui::NewLine();
	}
};

void ProceduralGeomPlugin::apply() {	
	OutputGeometryNode* output = (OutputGeometryNode*)m_nodes[0].get();
	Geometry geom(m_allocator);
	if (!output->getGeometry(0, &geom)) return;

	const Array<EntityRef>& selected = m_app.getWorldEditor().getSelectedEntities();
	if (selected.size() != 1) return;

	Universe* universe = m_app.getWorldEditor().getUniverse();
	RenderScene* scene = (RenderScene*)universe->getScene("renderer");
	if (!scene->hasProceduralGeometry(selected[0])) scene->createProceduralGeometry(selected[0]);

	Span<const u8> vertices((const u8*)geom.vertices.begin(), (u32)geom.vertices.byte_size());
	Span<const u8> indices((const u8*)geom.indices.begin(), (u32)geom.indices.byte_size());
	gpu::VertexDecl decl(geom.type);
	decl.addAttribute(0, 0, 3, gpu::AttributeType::FLOAT, 0);  // pos
	decl.addAttribute(1, 12, 2, gpu::AttributeType::FLOAT, 0); // uv
	decl.addAttribute(2, 20, 3, gpu::AttributeType::FLOAT, 0); // normal
	decl.addAttribute(3, 32, 3, gpu::AttributeType::FLOAT, 0); // tangent
	scene->setProceduralGeometry(selected[0], vertices, decl, indices, gpu::DataType::U32);
	scene->setProceduralGeometryMaterial(selected[0], Path(m_material));
}

Node* ProceduralGeomPlugin::addNode(NodeType type) {
	UniquePtr<Node> node;
	switch (type) {
		case NodeType::CONE: node = UniquePtr<ConeNode>::create(m_allocator); break;
		case NodeType::SPHERE: node = UniquePtr<SphereNode>::create(m_allocator); break;
		case NodeType::CUBE: node = UniquePtr<CubeNode>::create(m_allocator); break;
		case NodeType::OUTPUT: node = UniquePtr<OutputGeometryNode>::create(m_allocator); break;
		case NodeType::DISTRIBUTE_POINT_ON_FACES: node = UniquePtr<DistributePointsOnFacesNode>::create(m_allocator); break;
		case NodeType::GRID: node = UniquePtr<GridNode>::create(m_allocator); break;
		case NodeType::TRANSFORM: node = UniquePtr<TransformNode>::create(m_allocator); break;
		case NodeType::MERGE: node = UniquePtr<MergeNode>::create(m_allocator); break;
		default: ASSERT(false); return nullptr;
	}
	Node* res = node.get();
	res->m_plugin = this;
	res->m_allocator = &m_allocator;
	res->m_id = ++m_node_id_genereator;
	m_nodes.push(node.move());
	return res;
}

LUMIX_STUDIO_ENTRY(procedural_geom) {
	auto* plugin = LUMIX_NEW(app.getAllocator(), ProceduralGeomPlugin)(app);
	app.addPlugin(*plugin);
	return nullptr;
}

} // namespace Lumix

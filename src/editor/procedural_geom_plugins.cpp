#define LUMIX_NO_CUSTOM_CRT
#include "editor/asset_browser.h"
#include "editor/prefab_system.h"
#include "editor/settings.h"
#include "editor/studio_app.h"
#include "editor/utils.h"
#include "editor/world_editor.h"
#include "engine/allocator.h"
#include "engine/array.h"
#include "engine/core.h"
#include "engine/crt.h"
#include "engine/engine.h"
#include "engine/log.h"
#include "engine/os.h"
#include "engine/prefab.h"
#include "engine/resource_manager.h"
#include "engine/string.h"
#include "engine/world.h"
#include "renderer/material.h"
#include "renderer/model.h"
#include "renderer/render_scene.h"
#include <math.h>
#include <stdlib.h>

#include "imgui/imgui.h"


namespace Lumix {

struct ProceduralGeomGeneratorPlugin;

namespace {

using Link = NodeEditorLink;
enum { OUTPUT_FLAG = 1 << 31 };

static const ComponentType SPLINE_TYPE = reflection::getComponentType("spline");
static const ComponentType PROCEDURAL_GEOM_TYPE = reflection::getComponentType("procedural_geom");

enum class NodeType : u32 { 
	OUTPUT,
	CUBE,
	DISTRIBUTE_POINT_ON_FACES,
	PLACE_INSTANCES_AT_POINTS,
	GRID,
	TRANSFORM,
	MERGE,
	SPHERE,
	CONE,
	CYLINDER,
	SPLINE,
	EXTRUDE_ALONG,
	LINE,
	CIRCLE,
	POINT,
	INSTANTIATE_PREFAB,
	MODEL,
	SPIRAL,
	SCALE
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

struct Input {
	struct Node* node = nullptr;
	u16 output_idx;
	[[nodiscard]] bool getGeometry(Geometry* result) const;
	operator bool() const { return node != nullptr; }
};

struct Node : NodeEditorNode {
	virtual bool gui() = 0;
	virtual NodeType getType() const = 0;
	virtual bool getGeometry(u16 output_idx, Geometry* result) = 0;
	virtual void serialize(OutputMemoryStream& blob) const {}
	virtual void deserialize(InputMemoryStream& blob) {}

	Input getInput(u16 input) const;
	bool getInputGeometry(u16 input, Geometry* geometry) const;

	bool nodeGUI() override {
		ImGuiEx::BeginNode(m_id, m_pos, &m_selected);
		m_input_counter = 0;
		m_output_counter = 0;
		bool res = gui();
		ImGuiEx::EndNode();
		return res;
	}

	void inputSlot() {
		ImGuiEx::Pin(m_id | ((u32)m_input_counter << 16), true);
		++m_input_counter;
	}

	void outputSlot() {
		ImGuiEx::Pin(m_id | ((u32)m_output_counter << 16) | OUTPUT_FLAG, false);
		++m_output_counter;
	}

	struct EditorResource* m_resource;
	IAllocator* m_allocator;
	bool m_selected = false;
	u16 m_input_counter = 0;
	u16 m_output_counter = 0;
};

struct Header {
	static constexpr u32 MAGIC = '_PGM';
	u32 magic = MAGIC;
	u32 version = 0;
};

static u16 toNodeId(int id) {
	return u16(id);
}

static u16 toAttrIdx(int id) {
	return u16(u32(id) >> 16);
}

struct EditorResource {
	EditorResource(StudioApp& app, IAllocator& allocator)
		: m_app(app)
		, m_allocator(allocator)
		, m_nodes(allocator)
		, m_links(allocator)
	{}

	~EditorResource() {
		for (Node* node : m_nodes) LUMIX_DELETE(m_allocator, node);
	}

	void deleteSelectedNodes() {
		for (i32 i = m_nodes.size() - 1; i >= 0; --i) {
			Node* node = m_nodes[i];
			if (node->m_selected) {
				for (i32 j = m_links.size() - 1; j >= 0; --j) {
					if (m_links[j].getFromNode() == node->m_id || m_links[j].getToNode() == node->m_id) {
						m_links.erase(j);
					}
				}

				LUMIX_DELETE(m_allocator, node);
				m_nodes.swapAndPop(i);
			}
		}
	}

	void serialize(OutputMemoryStream& blob) {
		Header header;
		blob.write(header);
		blob.write(m_node_id_genereator);
		blob.writeString(m_material.c_str());
		blob.write(m_nodes.size());
		for (const Node* node : m_nodes) {
			blob.write(node->getType());
			blob.write(node->m_id);
			blob.write(node->m_pos);
			node->serialize(blob);
		}
		blob.write(m_links.size());
		blob.write(m_links.begin(), m_links.byte_size());
	}

	void deserialize(InputMemoryStream& blob, const char* path) {
		ASSERT(m_links.empty());
		ASSERT(m_nodes.empty());

		Header header;
		blob.read(header);
		if (header.magic != Header::MAGIC) {
			logError("Corrupted file ", path);
			return;
		}
		blob.read(m_node_id_genereator);
		m_material = blob.readString();
		u32 count;
		blob.read(count);
		m_nodes.reserve(count);
		for (u32 i = 0; i < count; ++i) {
			NodeType type;
			blob.read(type);
			Node* node = createNode(type, ImVec2(0, 0));
			blob.read(node->m_id);
			blob.read(node->m_pos);
			node->deserialize(blob);
		}
		blob.read(count);
		m_links.resize(count);
		blob.read(m_links.begin(), m_links.byte_size());
	}

	Node* createNode(NodeType type, ImVec2 pos);

	IAllocator& m_allocator;
	StudioApp& m_app;
	Array<Node*> m_nodes;
	Array<Link> m_links;
	Path m_material;
	u16 m_node_id_genereator = 1;
};

static const struct {
	char key;
	const char* label;
	NodeType type;
} TYPES[] = {
	{ 'O', "Circle", NodeType::CIRCLE },
	{ 0,   "Cone", NodeType::CONE },
	{ 'C', "Cube", NodeType::CUBE },
	{ 0,   "Cylinder", NodeType::CYLINDER },
	{ 'D', "Distribute points on faces", NodeType::DISTRIBUTE_POINT_ON_FACES },
	{ 'E', "Extrude along", NodeType::EXTRUDE_ALONG },
	{ 'G', "Grid", NodeType::GRID },
	{ 0, "Instantiate prefab", NodeType::INSTANTIATE_PREFAB },
	{ 'L', "Line", NodeType::LINE },
	{ 'M', "Merge", NodeType::MERGE },
	{ 0, "Model", NodeType::MODEL },
	{ 'I', "Place instances at points", NodeType::PLACE_INSTANCES_AT_POINTS },
	{ 'P', "Point", NodeType::POINT },
	{ 0, "Scale", NodeType::SCALE },
	{ 0, "Sphere", NodeType::SPHERE },
	{ 'Q', "Spiral", NodeType::SPIRAL},
	{ 'S', "Spline", NodeType::SPLINE },
	{ 'T', "Transform", NodeType::TRANSFORM },
};

} // anonymous namespace

static const ComponentType MODEL_INSTANCE_TYPE = reflection::getComponentType("model_instance");

struct ProceduralGeomGeneratorPlugin : StudioApp::GUIPlugin, NodeEditor {
	ProceduralGeomGeneratorPlugin(StudioApp& app)
		: m_app(app)
		, m_allocator(app.getAllocator())
		, m_recent_paths(app.getAllocator())
		, NodeEditor(app.getAllocator())
	{
		m_delete_action.init(ICON_FA_TRASH "Delete", "Procedural geometry editor delete", "proc_geom_editor_delete", ICON_FA_TRASH, os::Keycode::DEL, Action::Modifiers::NONE, true);
		m_delete_action.func.bind<&ProceduralGeomGeneratorPlugin::deleteSelectedNodes>(this);
		m_delete_action.plugin = this;

		m_undo_action.init(ICON_FA_UNDO "Undo", "Procedural geometry editor undo", "proc_geom_editor_undo", ICON_FA_UNDO, os::Keycode::Z, Action::Modifiers::CTRL, true);
		m_undo_action.func.bind<&ProceduralGeomGeneratorPlugin::undo>((SimpleUndoRedo*)this);
		m_undo_action.plugin = this;

		m_redo_action.init(ICON_FA_REDO "Redo", "Procedural geometry editor redo", "proc_geom_editor_redo", ICON_FA_REDO, os::Keycode::Z, Action::Modifiers::CTRL | Action::Modifiers::SHIFT, true);
		m_redo_action.func.bind<&ProceduralGeomGeneratorPlugin::redo>((SimpleUndoRedo*)this);
		m_redo_action.plugin = this;

		m_apply_action.init("Apply", "Procedural geometry editor apply", "proc_geom_editor_apply", ICON_FA_CHECK, os::Keycode::E, Action::Modifiers::CTRL, true);
		m_apply_action.func.bind<&ProceduralGeomGeneratorPlugin::apply>(this);
		m_apply_action.plugin = this;

		m_save_action.init(ICON_FA_SAVE "Save", "Procedural geometry editor save", "proc_geom_editor_save", ICON_FA_SAVE, os::Keycode::S, Action::Modifiers::CTRL, true);
		m_save_action.func.bind<&ProceduralGeomGeneratorPlugin::save>(this);
		m_save_action.plugin = this;

		m_toggle_ui.init("Procedural editor", "Toggle procedural editor", "procedural_editor", "", true);
		m_toggle_ui.func.bind<&ProceduralGeomGeneratorPlugin::toggleOpen>(this);
		m_toggle_ui.is_selected.bind<&ProceduralGeomGeneratorPlugin::isOpen>(this);
		
		app.addWindowAction(&m_toggle_ui);
		app.addAction(&m_undo_action);
		app.addAction(&m_redo_action);
		app.addAction(&m_delete_action);
		app.addAction(&m_apply_action);
		app.addAction(&m_save_action);

		newGraph();
	}

	~ProceduralGeomGeneratorPlugin(){
		m_app.removeAction(&m_toggle_ui);
		m_app.removeAction(&m_undo_action);
		m_app.removeAction(&m_redo_action);
		m_app.removeAction(&m_delete_action);
		m_app.removeAction(&m_apply_action);
		m_app.removeAction(&m_save_action);
		if (m_resource) LUMIX_DELETE(m_allocator, m_resource);
	}

	void pushUndo(u32 tag) override {
		if (m_autoapply) apply();
	}

	void onCanvasClicked(ImVec2 pos, i32 hovered_link) override {
		for (const auto& t : TYPES) {
			if (t.key && os::isKeyDown((os::Keycode)t.key)) {
				Node* node = addNode(t.type, pos, false);
				if (hovered_link >= 0) splitLink(m_resource->m_nodes.back(), m_resource->m_links, hovered_link);
				pushUndo(NO_MERGE_UNDO);
				break;
			}
		}
	}

	void onLinkDoubleClicked(Link& link, ImVec2 pos) override {}

	void onContextMenu(ImVec2 pos) override {
		Node* new_node = nullptr;
		for (const auto& t : TYPES) {
			if (ImGui::MenuItem(t.label)) new_node = addNode(t.type, pos, true);
		}
	}

	void deserialize(InputMemoryStream& blob) override {
		LUMIX_DELETE(m_allocator, m_resource);
		m_resource = LUMIX_NEW(m_allocator, EditorResource)(m_app, m_allocator);
		m_resource->deserialize(blob, "");
	}
	
	void serialize(OutputMemoryStream& blob) override {
		m_resource->serialize(blob);
	}

	void deleteSelectedNodes() {
		if (m_is_any_item_active) return;
		m_resource->deleteSelectedNodes();
		pushUndo(NO_MERGE_UNDO);
	}

	bool isOpen() const { return m_is_open; }
	void toggleOpen() { m_is_open = !m_is_open; }
	bool hasFocus() override { return m_has_focus; }

	void open(const char* path) {
		FileSystem& fs = m_app.getEngine().getFileSystem();
		OutputMemoryStream data(m_allocator);
		if (!fs.getContentSync(Path(path), data)) {
			logError("Failed to load ", path);
			return;
		}

		clear();

		InputMemoryStream blob(data);
		m_resource->deserialize(blob, path);

		m_path = path;
		pushRecent(path);
		pushUndo(NO_MERGE_UNDO);
	}

	void pushRecent(const char* path) {
		String p(path, m_app.getAllocator());
		m_recent_paths.eraseItems([&](const String& s) { return s == path; });
		m_recent_paths.push(static_cast<String&&>(p));
	}

	void clear() {
		LUMIX_DELETE(m_allocator, m_resource);
		m_resource = LUMIX_NEW(m_allocator, EditorResource)(m_app, m_allocator);
		clearUndoStack();
	}

	void save() {
		if (m_path.length() == 0) m_show_save_as = true;
		else saveAs(m_path.c_str());
	}

	void saveAs(const char* path) {
		FileSystem& fs = m_app.getEngine().getFileSystem();
		
		OutputMemoryStream blob(m_allocator);
		m_resource->serialize(blob);

		if (!fs.saveContentSync(Path(path), blob)) {
			logError("Could not save ", path);
			return;
		}

		m_path = path;
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

	void newGraph() {
		clear();
		m_path = "";
		addNode(NodeType::OUTPUT, ImVec2(100, 100), false);
		pushUndo(NO_MERGE_UNDO);
	}

	void onWindowGUI() override {
		m_has_focus = false;
		if (!m_is_open) return;

		ImGui::SetNextWindowSize(ImVec2(200, 200), ImGuiCond_FirstUseEver);
		if (ImGui::Begin("Procedural geometry", &m_is_open, ImGuiWindowFlags_MenuBar)) {
			m_has_focus = ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows);
			if (ImGui::BeginMenuBar()) {
				if (ImGui::BeginMenu("File")) {
					if (ImGui::MenuItem("New")) newGraph();
					if (ImGui::MenuItem("Open")) m_show_open = true;
					menuItem(m_save_action, true);
					if (ImGui::MenuItem("Save As")) m_show_save_as = true;
					menuItem(m_apply_action, canApply());
					ImGui::MenuItem("Autoapply", nullptr, &m_autoapply);
					if (ImGui::BeginMenu("Recent", !m_recent_paths.empty())) {
						for (const String& path : m_recent_paths) {
							if (ImGui::MenuItem(path.c_str())) open(path.c_str());
						}
						ImGui::EndMenu();
					}
					ImGui::EndMenu();
				}
				if (ImGui::BeginMenu("Edit")) {
					menuItem(m_undo_action, canUndo());
					menuItem(m_redo_action, canRedo());
					ImGui::EndMenu();
				}
				ImGui::EndMenuBar();
			}

			FileSelector& fs = m_app.getFileSelector();
			if (fs.gui("Open", &m_show_open, "pgm", false)) open(fs.getPath());
			if (fs.gui("Save As", &m_show_save_as, "pgm", true)) saveAs(fs.getPath());

			Span span(m_resource->m_material.beginUpdate(), m_resource->m_material.capacity());
			m_app.getAssetBrowser().resourceInput("material", span, Material::TYPE);
			m_resource->m_material.endUpdate();	

			nodeEditorGUI(m_resource->m_nodes, m_resource->m_links);
		}
		ImGui::End();
	}
	

	bool canApply() {
		return m_resource->m_material.length() != 0 && !m_app.getWorldEditor().getSelectedEntities().empty();
	}

	void apply();
	Node* addNode(NodeType type, ImVec2 pos, bool save_undo);

	const char* getName() const override { return "procedural_geom_generator"; }
	
	IAllocator& m_allocator;
	StudioApp& m_app;
	bool m_is_open = true;
	Array<String> m_recent_paths;
	bool m_autoapply = false;
	bool m_has_focus = false;
	Action m_toggle_ui;
	Action m_delete_action;
	Action m_undo_action;
	Action m_redo_action;
	Action m_apply_action;
	Action m_save_action;
	Path m_path;
	EditorResource* m_resource = nullptr;
	bool m_show_save_as = false;
	bool m_show_open = false;
};

template <typename F>
static void	forEachInput(const EditorResource& resource, int node_id, const F& f) {
	for (const Link& link : resource.m_links) {
		if (toNodeId(link.to) == node_id) {
			const int iter = resource.m_nodes.find([&](const Node* node) { return node->m_id == toNodeId(link.from); }); 
			Node* from = resource.m_nodes[iter];
			const u16 from_attr = toAttrIdx(link.from);
			const u16 to_attr = toAttrIdx(link.to);
			f(from, from_attr, to_attr, u32(&link - resource.m_links.begin()));
		}
	}
}

bool Input::getGeometry(Geometry* result) const { return node->getGeometry(output_idx, result); }


bool Node::getInputGeometry(u16 input_idx, Geometry* geometry) const {
	const Input input = getInput(input_idx);
	if (!input) return false;
	return input.getGeometry(geometry);
}

Input Node::getInput(u16 input_idx) const {
	Input res;
	forEachInput(*m_resource, m_id, [&](Node* from, u16 from_attr, u16 to_attr, u32 link_idx){
		if (to_attr == input_idx) {
			res.output_idx = from_attr;
			res.node = from;
		}
	});
	return res;
}

struct CircleNode : Node {
	NodeType getType() const override { return NodeType::CIRCLE; }

	bool hasInputPins() const override { return false; }
	bool hasOutputPins() const override { return true; }

	bool gui() override {
		ImGuiEx::NodeTitle("Circle");
		outputSlot();
		bool res = ImGui::DragFloat("Radius", &radius);
		res = ImGui::DragInt("Subdivision", (i32*)&subdivision) || res;
		return res;
	}

	bool getGeometry(u16 output_idx, Geometry* result) override {
		result->vertices.reserve(subdivision + 1);
		for (u32 i = 0; i <= subdivision; ++i) {
			const float a = i / (float)subdivision * PI * 2;
			Geometry::Vertex& v = result->vertices.emplace();
			v.position = Vec3(cosf(a) * radius, sinf(a) * radius, 0);
			v.normal = normalize(v.position);
			v.tangent = Vec3(-v.position.y, v.position.x, 0);
			v.uv = Vec2(i / (float)subdivision, 0);
		}
		result->type = gpu::PrimitiveType::LINES;
		return true;
	}
	
	void serialize(OutputMemoryStream& blob) const override {
		blob.write(radius);
		blob.write(subdivision);
	}

	void deserialize(InputMemoryStream& blob) override {
		blob.read(radius);
		blob.read(subdivision);
	}

	u32 subdivision = 32;
	float radius = 1.f;
};

struct LineNode : Node {
	NodeType getType() const override { return NodeType::LINE; }

	bool hasInputPins() const override { return false; }
	bool hasOutputPins() const override { return true; }

	bool gui() override {
		ImGuiEx::NodeTitle("Line");
		outputSlot();
		bool res = ImGui::DragFloat("Size", &size);
		res = ImGui::DragInt("Subdivision", (i32*)&subdivision) || res;
		return res;
	}

	bool getGeometry(u16 output_idx, Geometry* result) override {
		result->vertices.reserve(subdivision + 1);
		for (u32 i = 0; i <= subdivision; ++i) {
			Geometry::Vertex& v = result->vertices.emplace();
			v.position = Vec3(-size * (i / (float)subdivision * 2 - 1), 0, 0);
			v.uv = Vec2(i / (float)subdivision, 0);
			v.normal = Vec3(0, 1, 0);
			v.tangent = Vec3(1, 0, 0);
		}
		result->type = gpu::PrimitiveType::LINES;
		return true;
	}

	void serialize(OutputMemoryStream& blob) const override {
		blob.write(size);
		blob.write(subdivision);
	}

	void deserialize(InputMemoryStream& blob) override {
		blob.read(size);
		blob.read(subdivision);
	}

	float size = 1.f;
	u32 subdivision = 16;
};

struct PlaceInstancesAtPoints : Node {
	NodeType getType() const override { return NodeType::PLACE_INSTANCES_AT_POINTS; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }

	bool gui() override {
		ImGuiEx::NodeTitle("Place instances at points");
		ImGui::BeginGroup();
		inputSlot(); ImGui::TextUnformatted("Points");
		inputSlot(); ImGui::TextUnformatted("Instance");
		ImGui::EndGroup();
		ImGui::SameLine();
		outputSlot();
		return false;
	}

	bool getGeometry(u16 output_idx, Geometry* result) override {
		Geometry points(*m_allocator);
		Geometry instance(*m_allocator);

		if (!getInputGeometry(0, &points)) return false;
		if (!getInputGeometry(1, &instance)) return false;
		if (instance.type != gpu::PrimitiveType::TRIANGLES) return false;

		result->vertices.reserve(points.vertices.size() * instance.vertices.size());
		result->indices.reserve(points.indices.size() * instance.indices.size());

		for (const Geometry::Vertex& point : points.vertices) {
			Matrix mtx;
			const Vec3 dir = cross(point.normal, point.tangent);
			mtx.lookAt(point.position, point.position + dir, point.normal);
			mtx = mtx.fastInverted();

			const u32 index_offset = result->vertices.size();

			for (const Geometry::Vertex& src_v : instance.vertices) {
				Geometry::Vertex& v = result->vertices.emplace();
				v = src_v;
				v.position = mtx.transformPoint(v.position);
				v.tangent = mtx.transformVector(v.tangent);
				v.normal = mtx.transformVector(v.normal);
			}

			for (u32 idx : instance.indices) {
				result->indices.push(index_offset + idx);
			}
		}
		
		result->type = gpu::PrimitiveType::TRIANGLES;
		return true;
	}
};

struct ModelNode : Node {
	~ModelNode() {
		if (m_model) m_model->decRefCount();
	}

	NodeType getType() const override { return NodeType::MODEL; }

	bool hasInputPins() const override { return false; }
	bool hasOutputPins() const override { return true; }

	void serialize(OutputMemoryStream& blob) const override {
		blob.writeString(m_model ? m_model->getPath().c_str() : "");
	}

	void deserialize(InputMemoryStream& blob) override {
		const char* path = blob.readString();
		if (path[0]) {
			ResourceManagerHub& rm = m_resource->m_app.getEngine().getResourceManager();
			m_model = rm.load<Model>(Path(path));
		}
		else {
			m_model = nullptr;
		}
	}
	
	bool gui() override {
		ImGuiEx::NodeTitle("Model");
		outputSlot();
		char path[LUMIX_MAX_PATH];
		copyString(path, m_model ? m_model->getPath().c_str() : "");
		if (m_resource->m_app.getAssetBrowser().resourceInput("Asset", Span(path), Model::TYPE, 150)) {
			if (m_model) m_model->decRefCount();
			ResourceManagerHub& rm = m_resource->m_app.getEngine().getResourceManager();
			m_model = rm.load<Model>(Path(path));
			return true;
		}
		return false;
	}

	bool getGeometry(u16 output_idx, Geometry* result) override {
		if (!m_model) return false;
		if (!m_model->isReady()) return false;

		for (u32 i = 0, ci= m_model->getMeshCount(); i < ci; ++i) {
			const Mesh& mesh = m_model->getMesh(i);
			
			const u32 indices_offset = result->vertices.size();

			for (const Vec3& vin : mesh.vertices) {
				Geometry::Vertex& vout = result->vertices.emplace();
				vout.position = vin;
				// TODO get actual data
				vout.uv = Vec2(0, 0);
				vout.normal = Vec3(0, 1, 0);
				vout.tangent = Vec3(1, 0, 0);
			}

			const bool are_indices_16bit = mesh.areIndices16();
			const u32 index_size = are_indices_16bit ? 2 : 4;
			for (u32 j = 0, c = mesh.indices_count; j < c; ++j) {
				u32 idx = are_indices_16bit ? *(u16*)(mesh.indices.data() + j * 2) : *(u32*)(mesh.indices.data() + j * 4);
				result->indices.push(idx + indices_offset);
			}
		}

		result->type = gpu::PrimitiveType::TRIANGLES;
		return true;
	}

	Model* m_model = nullptr;
};

struct InstantiatePrefabNode : Node {
	~InstantiatePrefabNode() {
		if (m_prefab) m_prefab->decRefCount();
	}

	NodeType getType() const override { return NodeType::INSTANTIATE_PREFAB; }

	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return false; }

	void serialize(OutputMemoryStream& blob) const override {
		blob.writeString(m_prefab ? m_prefab->getPath().c_str() : "");
	}

	void deserialize(InputMemoryStream& blob) override {
		const char* path = blob.readString();
		if (path[0]) {
			ResourceManagerHub& rm = m_resource->m_app.getEngine().getResourceManager();
			m_prefab = rm.load<PrefabResource>(Path(path));
		}
		else {
			m_prefab = nullptr;
		}
	}

	bool gui() override {
		ImGuiEx::NodeTitle("Instantiate prefab");
		inputSlot();
		ImGui::TextUnformatted("Points");
		char path[LUMIX_MAX_PATH];
		copyString(path, m_prefab ? m_prefab->getPath().c_str() : "");
		ResourceManagerHub& rm = m_resource->m_app.getEngine().getResourceManager();
		if (m_resource->m_app.getAssetBrowser().resourceInput("Prefab", Span(path), PrefabResource::TYPE, 150)) {
			if (m_prefab) m_prefab->decRefCount();
			m_prefab = rm.load<PrefabResource>(Path(path));
			return true;
		}
		if (ImGui::Button("Instantiate")) {
			WorldEditor& editor = m_resource->m_app.getWorldEditor();
			const Array<EntityRef>& selected = editor.getSelectedEntities();
			World* world = editor.getWorld();
			while (EntityPtr child = world->getFirstChild(selected[0])) {
				world->destroyEntity(*child);
			}
			if (selected.size() == 1) instantiate(selected[0]);
		}
		return false;
	}
	
	void instantiate(EntityRef parent) const {
		if (!m_prefab || !m_prefab->isReady()) return;
		
		const Input input = getInput(0);
		if (!input) return;

		WorldEditor& editor = m_resource->m_app.getWorldEditor();
		World& world = *editor.getWorld();
		
		const Transform& transform = world.getTransform(parent);
		Geometry points(*m_allocator);
		if (!input.getGeometry(&points)) return;

		PrefabSystem& prefab_system = m_resource->m_app.getWorldEditor().getPrefabSystem();
		for (const Geometry::Vertex& v : points.vertices) {
			const DVec3 p = transform.transform(v.position);
			// TODO rotation
			const EntityPtr e = prefab_system.instantiatePrefab(*m_prefab, p, transform.rot, transform.scale);
			if (e.isValid()) world.setParent(parent, *e);
		}
	}

	bool getGeometry(u16 output_idx, Geometry* result) override {
		return false;
	}

	PrefabResource* m_prefab = nullptr;
};

struct SplineIterator {
	SplineIterator(Span<const Vec3> points) : points(points) {}

	void move(float delta) { t += delta; }
	bool isEnd() { return u32(t) >= points.length() - 2; }
	Vec3 getNormal() const {
		const u32 segment = u32(t);
		Vec3 p0 = points[segment + 0];
		Vec3 p1 = points[segment + 1];
		Vec3 p2 = points[segment + 2];
		Vec3 n = normalize(cross(p1 - p0, p1 - p2));
		if (n.y < 0) n *= -1;
		return n;
	}

	Vec3 getDir() const {
		const u32 segment = u32(t);
		float rel_t = t - segment;
		Vec3 p0 = points[segment + 0];
		Vec3 p1 = points[segment + 1];
		Vec3 p2 = points[segment + 2];
		return lerp(p1 - p0, p2 - p1, rel_t);
	}

	Vec3 getPosition() const {
		const u32 segment = u32(t);
		float rel_t = t - segment;
		Vec3 p0 = points[segment + 0];
		Vec3 p1 = points[segment + 1];
		Vec3 p2 = points[segment + 2];
		p0 = (p1 + p0) * 0.5f;
		p2 = (p1 + p2) * 0.5f;

		return lerp(lerp(p0, p1, rel_t), lerp(p1, p2, rel_t), rel_t);
	}

	float t = 0;

	Span<const Vec3> points;
};

struct PointNode : Node {
	NodeType getType() const override { return NodeType::POINT; }

	bool hasInputPins() const override { return false; }
	bool hasOutputPins() const override { return true; }

	void serialize(OutputMemoryStream& blob) const override {
		blob.write(position);
	}

	void deserialize(InputMemoryStream& blob) override {
		blob.read(position);
	}

	bool gui() override {
		ImGuiEx::NodeTitle("Point");
		outputSlot();
		bool res = ImGui::DragFloat("X", &position.x);
		res = ImGui::DragFloat("Y", &position.y) || res;
		res = ImGui::DragFloat("Z", &position.z) || res;
		return res;
	}

	bool getGeometry(u16 output_idx, Geometry* result) override {
		Geometry::Vertex& v = result->vertices.emplace();
		v.position = position;
		v.normal = Vec3(0, 1, 0);
		v.tangent = Vec3(1, 0, 0);
		v.uv = Vec2(0, 0);
		result->type = gpu::PrimitiveType::POINTS;
		return true;
	}

	Vec3 position = Vec3(0, 0, 0);
};

struct SpiralNode : Node {
	NodeType getType() const override { return NodeType::SPIRAL; }
	
	void serialize(OutputMemoryStream& blob) const override {
		blob.write(height);
		blob.write(radius_start);
		blob.write(radius_end);
		blob.write(turns);
		blob.write(resolution);
	}

	void deserialize(InputMemoryStream& blob) override {
		blob.read(height);
		blob.read(radius_start);
		blob.read(radius_end);
		blob.read(turns);
		blob.read(resolution);
	}

	bool hasInputPins() const override { return false; }
	bool hasOutputPins() const override { return true; }
	
	bool gui() override {
		ImGuiEx::NodeTitle("Spiral");
		outputSlot();
		bool res = ImGui::DragFloat("Height", &height, 1, FLT_MIN, FLT_MAX);
		res = ImGui::DragFloat("Start radius", &radius_start, 1, FLT_MIN, FLT_MAX) || res;
		res = ImGui::DragFloat("End radius", &radius_end, 1, FLT_MIN, FLT_MAX) || res;
		res = ImGui::DragFloat("Turns", &turns, 1, FLT_MIN, FLT_MAX) || res;
		res = ImGui::DragInt("Resolution", (i32*)&resolution, 1, 1, 999999) || res;
		return res;
	}
	
	bool getGeometry(u16 output_idx, Geometry* result) override {
		const u32 count = u32(turns * resolution + 0.5f);
		if (count == 0) return false;
		result->vertices.reserve(count);
		
		for (u32 i = 0; i < count; ++i) {
			const float t = i / float(count - 1);
			const float radius = radius_start * (1 - t) + radius_end * t; 
			const float h = height * t;
			const float angle = (turns * t) * 2 * PI;
			Geometry::Vertex& v = result->vertices.emplace();
			v.position = Vec3(cosf(angle) * radius, sinf(angle) * radius, h);
			v.uv = Vec2(h, 0);
			v.normal = normalize(Vec3(v.position.x, v.position.y, 0));
			v.tangent = Vec3(-v.normal.y, v.normal.x, 0);
		}
		result->type = gpu::PrimitiveType::POINTS;
		return true;
	}

	float height = 1;
	float radius_start = 1;
	float radius_end = 1;
	float turns = 4;
	u32 resolution = 16;
};

struct SplineNode : Node {
	NodeType getType() const override { return NodeType::SPLINE; }

	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }
	
	bool gui() override {
		ImGuiEx::NodeTitle("Spline");
		outputSlot();
		return ImGui::DragFloat("Step", &step);
	}

	bool getGeometry(u16 output_idx, Geometry* result) override {
		WorldEditor& editor = m_resource->m_app.getWorldEditor();
		const Array<EntityRef>& selected = editor.getSelectedEntities();
		if (selected.size() != 1) return false;

		World& world = *editor.getWorld();
		if (!world.hasComponent(selected[0], SPLINE_TYPE)) return false;

		CoreScene* core_scene = (CoreScene*)world.getScene(SPLINE_TYPE);
		const Spline& spline = core_scene->getSpline(selected[0]);
		if (spline.points.empty()) return false;


		SplineIterator iterator(spline.points);
		float d = 0;
		result->vertices.reserve(512);
		Vec3 prev_p = spline.points[0];
		while (!iterator.isEnd()) {
			const Vec3 dir = normalize(iterator.getDir());
			Geometry::Vertex& v = result->vertices.emplace();
			v.position = iterator.getPosition();
			v.normal = iterator.getNormal();
			v.tangent = dir;
			v.uv = Vec2(0, d);
			// TODO actual step is not uniform
			iterator.move(step);
			d += length(v.position - prev_p);
			prev_p = v.position;
		}
		result->type = gpu::PrimitiveType::POINTS;
		return true;
	}

	void serialize(OutputMemoryStream& blob) const override {
		blob.write(step);
	}

	void deserialize(InputMemoryStream& blob) override {
		blob.read(step);
	}

	float step = 0.1f;
};

struct ExtrudeAlongNode : Node {
	NodeType getType() const override { return NodeType::EXTRUDE_ALONG; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }

	bool gui() override {
		ImGuiEx::NodeTitle("Extrude along");
		ImGui::BeginGroup();
		inputSlot(); ImGui::TextUnformatted("Profile");
		inputSlot(); ImGui::TextUnformatted("Path");
		ImGui::EndGroup();
		ImGui::SameLine();
		outputSlot();
		return false;
	}

	bool getGeometry(u16 output_idx, Geometry* result) override {
		Geometry profile(*m_allocator);
		Geometry path(*m_allocator);

		if (!getInputGeometry(0, &profile)) return false;
		if (!getInputGeometry(1, &path)) return false;
		if (path.type != gpu::PrimitiveType::LINES && path.type != gpu::PrimitiveType::POINTS) return false;
		
		switch (profile.type) {
			case gpu::PrimitiveType::POINTS:
			case gpu::PrimitiveType::LINES: {
				result->vertices.reserve(path.vertices.size() * profile.vertices.size());
				
				float d = 0;
				u32 rows = 0;
				Vec3 prev_p = path.vertices[0].position;
				
				for (Geometry::Vertex& path_v : path.vertices) {
					++rows;
					const Vec3 p = path_v.position;
		
					const Vec3 up = path_v.normal;
					const Vec3 dir = path_v.tangent;
					const Vec3 side = normalize(cross(up, dir));
					d += length(p - prev_p);
		
					const u32 w = profile.vertices.size();
					for (u32 i = 0; i < w; ++i) {
						Geometry::Vertex& v = result->vertices.emplace();
						v.position = p + side * profile.vertices[i].position.x + up * profile.vertices[i].position.y;
						v.uv = profile.vertices[i].uv;
						v.uv.y = d;
						v.normal = profile.vertices[i].normal;
						v.normal = side * v.normal.x + up * v.normal.y + dir * v.normal.z;
						v.tangent = profile.vertices[i].tangent;
						v.tangent = side * v.tangent.x + up * v.tangent.y + dir * v.tangent.z;
					}
		
					if (rows > 1) {
						const u32 offset = result->vertices.size() - 2 * w;
						for (u32 i = 0; i < w - 1; ++i) {
							result->indices.push(offset + i);
							result->indices.push(offset + i + 1);
							result->indices.push(offset + i + w);
		
							result->indices.push(offset + i + w + 1);
							result->indices.push(offset + i + w);
							result->indices.push(offset + i + 1);
						}
					}
					prev_p = p;
				}
		
				result->type = gpu::PrimitiveType::TRIANGLES;
				return true;
			}
			default: 
				return false;
		}
	}
};

struct CylinderNode : Node {
	NodeType getType() const override { return NodeType::CYLINDER; }
	bool hasInputPins() const override { return false; }
	bool hasOutputPins() const override { return true; }
	
	void serialize(OutputMemoryStream& blob) const override {
		blob.write(subdivision);
		blob.write(radius);
		blob.write(height);
	}

	void deserialize(InputMemoryStream& blob) override {
		blob.read(subdivision);
		blob.read(radius);
		blob.read(height);
	}
	
	bool getGeometry(u16 output_idx, Geometry* result) override {
		result->vertices.reserve(2 * subdivision + 4);

		{
			Geometry::Vertex& v1 = result->vertices.emplace();
			v1.position = Vec3(0, 0, 0.5f * height);
			v1.normal = Vec3(0, 0, 1);
			v1.tangent = Vec3(1, 0, 0);
			v1.uv = Vec2(0, 0);
	
			Geometry::Vertex& v2 = result->vertices.emplace();
			v2.position = Vec3(0, 0, -0.5f * height);
			v2.normal = Vec3(0, 0, -1);
			v2.tangent = Vec3(-1, 0, 0);
			v2.uv = Vec2(0, 0);
		}

		for (u32 i = 0; i < subdivision; ++i) {
			const float a = i / float(subdivision) * PI * 2;
			Geometry::Vertex& v = result->vertices.emplace();
			v.position = Vec3(cosf(a) * radius, sinf(a) * radius, 0.5f * height);
			v.normal = Vec3(0, 0, 1);
			v.tangent = Vec3(1, 0, 0);
			v.uv = Vec2(v.position.x, v.position.y);
		}

		for (u32 i = 0; i < subdivision; ++i) {
			const float a = i / float(subdivision) * PI * 2;
			Geometry::Vertex& v = result->vertices.emplace();
			v.position = Vec3(cosf(a) * radius, sinf(a) * radius, -0.5f * height);
			v.normal = Vec3(0, 0, -1);
			v.tangent = Vec3(-1, 0, 0);
			v.uv = Vec2(v.position.x, v.position.y);
		}

		for (u32 i = 0; i < subdivision; ++i) {
			const float a = i / float(subdivision) * PI * 2;
			Geometry::Vertex& v = result->vertices.emplace();
			v.position = Vec3(cosf(a) * radius, sinf(a) * radius, 0.5f * height);
			v.normal = normalize(Vec3(v.position.x, v.position.y, 0));
			v.tangent = Vec3(-1, 0, 0);
			v.uv = Vec2(v.position.x, v.position.y);

			Geometry::Vertex& v2 = result->vertices.emplace();
			v2.position = Vec3(cosf(a) * radius, sinf(a) * radius, -0.5f * height);
			v2.normal = normalize(Vec3(v2.position.x, v2.position.y, 0));
			v2.tangent = Vec3(-1, 0, 0);
			v2.uv = Vec2(v2.position.x, v2.position.y);
		}


		result->indices.reserve(subdivision * 3 * 2);
		for (u32 i = 0; i < subdivision; ++i) {
			result->indices.push(0);
			result->indices.push(2 + i);
			result->indices.push(2 + (i + 1) % subdivision);
		}

		for (u32 i = 0; i < subdivision; ++i) {
			result->indices.push(1);
			result->indices.push(2 + subdivision + (i + 1) % subdivision);
			result->indices.push(2 + subdivision + i);
		}

		for (u32 i = 0; i < subdivision; ++i) {
			result->indices.push(2 + 2 * subdivision + 2 * i);
			result->indices.push(2 + 2 * subdivision + ((2 * i) + 1) % (2 * subdivision));
			result->indices.push(2 + 2 * subdivision + ((2 * i) + 2) % (2 * subdivision));

			result->indices.push(2 + 2 * subdivision + ((2 * i) + 2) % (2 * subdivision));
			result->indices.push(2 + 2 * subdivision + ((2 * i) + 1) % (2 * subdivision));
			result->indices.push(2 + 2 * subdivision + ((2 * i) + 3) % (2 * subdivision));
		}

		result->type = gpu::PrimitiveType::TRIANGLES;
		return true;
	};

	bool gui() override {
		ImGuiEx::NodeTitle("Cylinder");
		outputSlot();
		bool res = ImGui::DragInt("Subdivision", (i32*)&subdivision);
		res = ImGui::DragFloat("Radius", &radius) || res;
		res = ImGui::DragFloat("Height", &height) || res;
		return res;
	}
	
	u32 subdivision = 32;
	float radius = 1;
	float height = 1;
};

struct ConeNode : Node {
	NodeType getType() const override { return NodeType::CONE; }
	bool hasInputPins() const override { return false; }
	bool hasOutputPins() const override { return true; }
	
	void serialize(OutputMemoryStream& blob) const override {
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

	bool gui() override {
		ImGuiEx::NodeTitle("Cone");
		outputSlot();
		bool res = ImGui::DragInt("Subdivision", (i32*)&subdivision);
		res = ImGui::DragFloat("Base size", &base_size) || res;
		res = ImGui::DragFloat("Height", &height) || res;
		return res;
	}	

	u32 subdivision = 30;
	float base_size = 1;
	float height = 3;
};

struct SphereNode : Node {
	NodeType getType() const override { return NodeType::SPHERE; }
	bool hasInputPins() const override { return false; }
	bool hasOutputPins() const override { return true; }
	
	void serialize(OutputMemoryStream& blob) const override {
		blob.write(size);
		blob.write(subdivision);
	}

	void deserialize(InputMemoryStream& blob) override {
		blob.read(size);
		blob.read(subdivision);
	}
	
	bool getGeometry(u16 output_idx, Geometry* result) override {
		result->vertices.reserve(6 * subdivision * subdivision);
		result->indices.reserve(12 * (subdivision - 1));

		auto push_side = [result, this](u32 coord0, u32 coord1, u32 coord2, float coord2_val){
			const u32 offset = result->vertices.size();
			for (u32 j = 0; j < subdivision; ++j) {
				for (u32 i = 0; i < subdivision; ++i) {
					Geometry::Vertex& v = result->vertices.emplace();
					v.position[coord0] = i / float(subdivision - 1) * 2 - 1;
					v.position[coord1] = j / float(subdivision - 1) * 2 - 1;
					v.position[coord2] = coord2_val;
					v.position = normalize(v.position);
					// TODO proper UV and tangent
					v.uv = { i / float(subdivision - 1), j / float(subdivision - 1) };
					v.normal = v.position;
					v.tangent = normalize(Vec3(0, v.position.z, v.position.y));
					v.position *= size;
				}
			}
			for (u32 j = 0; j < subdivision - 1; ++j) {
				for (u32 i = 0; i < subdivision - 1; ++i) {
					result->indices.push(offset + i + j * subdivision);
					result->indices.push(offset + i + 1 + j * subdivision);
					result->indices.push(offset + i + (j + 1) * subdivision);

					result->indices.push(offset + i + (j + 1) * subdivision);
					result->indices.push(offset + i + 1 + j * subdivision);
					result->indices.push(offset + i + 1 + (j + 1) * subdivision);
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

	bool gui() override {
		ImGuiEx::NodeTitle("Sphere");
		outputSlot();
		bool res = ImGui::DragInt("Subdivision", (i32*)&subdivision);
		res = ImGui::DragFloat("Size", &size) || res;
		return res;
	}

	u32 subdivision = 10;
	float size = 1.f;
};

struct CubeNode : Node {
	NodeType getType() const override { return NodeType::CUBE; }
	bool hasInputPins() const override { return false; }
	bool hasOutputPins() const override { return true; }

	void serialize(OutputMemoryStream& blob) const override {
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

	bool gui() override {
		ImGuiEx::NodeTitle("Cube");
		outputSlot();
		bool res = ImGui::DragInt("Vertices X", &size.x);
		res = ImGui::DragInt("Vertices Y", &size.y) || res;
		res = ImGui::DragInt("Vertices Z", &size.z) || res;
		return res;
	}

	IVec3 size = IVec3(2);	
};

struct MergeNode : Node {
	NodeType getType() const override { return NodeType::MERGE; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }

	bool getGeometry(u16 output_idx, Geometry* result) override {
		if (!getInputGeometry(0, result)) return false;

		const Input input1 = getInput(1);
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

	bool gui() override {
		ImGuiEx::NodeTitle("Merge");
		ImGui::BeginGroup();
		inputSlot(); ImGui::TextUnformatted("A");
		inputSlot(); ImGui::TextUnformatted("B");
		ImGui::EndGroup();
		ImGui::SameLine();		
		outputSlot();
		return false;
	}
};

struct TransformNode : Node {
	NodeType getType() const override { return NodeType::TRANSFORM; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }

	void serialize(OutputMemoryStream& blob) const override {
		blob.write(translation);
		blob.write(euler);
		blob.write(scale);
	}
	
	void deserialize(InputMemoryStream& blob) override {
		blob.read(translation);
		blob.read(euler);
		blob.read(scale);
	}

	bool getGeometry(u16 output_idx, Geometry* result) override {
		if (!getInputGeometry(0, result)) return false;

		Quat rotation;
		rotation.fromEuler(euler);

		// TODO transform normal and tangent
		for (Geometry::Vertex& v : result->vertices) {
			v.position = rotation.rotate(v.position) * scale + translation;
		}

		return true;
	}

	bool gui() override {
		ImGuiEx::NodeTitle("Transform");
		inputSlot();
		ImGui::BeginGroup();
		bool res = ImGui::DragFloat3("Translation", &translation.x);
		res = ImGuiEx::InputRotation("Rotation", &euler.x) || res;
		res = ImGui::DragFloat3("Scale", &scale.x) || res;
		ImGui::EndGroup();
		ImGui::SameLine();
		outputSlot();
		return res;
	}

	Vec3 translation = Vec3::ZERO;
	Vec3 euler = Vec3::ZERO;
	Vec3 scale = Vec3(1);
};

struct ScaleNode : Node {
	NodeType getType() const override { return NodeType::SCALE; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }

	void serialize(OutputMemoryStream& blob) const override {
		blob.write(scale);
	}
	
	void deserialize(InputMemoryStream& blob) override {
		blob.read(scale);
	}

	bool getGeometry(u16 output_idx, Geometry* result) override {
		if (!getInputGeometry(0, result)) return false;

		// TODO transform normal and tangent
		for (Geometry::Vertex& v : result->vertices) {
			v.position = v.position * scale;
		}

		return true;
	}

	bool gui() override {
		ImGuiEx::NodeTitle("Scale");
		inputSlot();
		ImGui::BeginGroup();
		bool res = ImGui::DragFloat("X", &scale.x);
		res = ImGui::DragFloat("Y", &scale.y) || res;
		res = ImGui::DragFloat("Z", &scale.z) || res;
		ImGui::EndGroup();
		ImGui::SameLine();
		outputSlot();
		return res;
	}

	Vec3 scale = Vec3(1);
};
struct GridNode : Node {
	NodeType getType() const override { return NodeType::GRID; }
	bool hasInputPins() const override { return false; }
	bool hasOutputPins() const override { return true; }
	
	void serialize(OutputMemoryStream& blob) const override {
		blob.write(m_cols);
		blob.write(m_rows);
	}
	
	void deserialize(InputMemoryStream& blob) override {
		blob.read(m_cols);
		blob.read(m_rows);
	}

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

	bool gui() override {
		ImGuiEx::NodeTitle("Grid");
		outputSlot();
		bool res = ImGui::DragInt("Columns", &m_cols);
		res = ImGui::DragInt("Rows", &m_rows) || res;
		return res;
	}

	i32 m_cols = 10;
	i32 m_rows = 10;
};

struct DistributePointsOnFacesNode : Node {
	NodeType getType() const override { return NodeType::DISTRIBUTE_POINT_ON_FACES; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }
	
	void serialize(OutputMemoryStream& blob) const override {
		blob.write(density);
	}
	
	void deserialize(InputMemoryStream& blob) override {
		blob.read(density);
	}

	bool getGeometry(u16 output_idx, Geometry* result) override {
		Geometry geom(*m_allocator);

		if (!getInputGeometry(0, &geom)) return false;
		if (geom.type != gpu::PrimitiveType::TRIANGLES) return false;

		float total_area = 0;
		for (u32 i = 0; i < (u32)geom.indices.size(); i += 3) {
			const u32 idx0 = geom.indices[i];
			const u32 idx1 = geom.indices[i + 1];
			const u32 idx2 = geom.indices[i + 2];
			const Vec3 v0 = geom.vertices[idx1].position - geom.vertices[idx0].position;
			const Vec3 v1 = geom.vertices[idx2].position - geom.vertices[idx0].position;
			total_area += length(cross(v0, v1));
		}

		result->vertices.reserve(u32(density * total_area));
		u32 N = u32(density * total_area);
		for (u32 j = 0; j < N; ++j) {
			float r = halton(j, 2) * total_area;
			for (u32 i = 0, c = geom.indices.size(); i < c; i += 3) {
				const u32 idx0 = geom.indices[i];
				const u32 idx1 = geom.indices[i + 1];
				const u32 idx2 = geom.indices[i + 2];
				const Vec3 v0 = geom.vertices[idx1].position - geom.vertices[idx0].position;
				const Vec3 v1 = geom.vertices[idx2].position - geom.vertices[idx0].position;
				const float area = length(cross(v0, v1));
				r -= area;
				if (r < 0) {
					Vec2 uv(randFloat(), randFloat());
					if (uv.x + uv.y > 1) {
						uv.x = 1 - uv.x;
						uv.y = 1 - uv.y;
					}
					Geometry::Vertex& v = result->vertices.emplace();
					v.position = geom.vertices[idx0].position + uv.x * v0 + uv.y * v1;
					v.normal = normalize(cross(v0, v1));
					v.tangent = v0;
					break;
				}
			}
		}

		result->type = gpu::PrimitiveType::POINTS;
		return true;
	}

	bool gui() override {
		ImGuiEx::NodeTitle("Distribute points on faces");
		inputSlot();
		bool res = ImGui::DragFloat("Density", &density);
		ImGui::SameLine();
		outputSlot();
		return res;
	}

	float density = 1.f;
};

struct OutputGeometryNode : Node {
	NodeType getType() const override { return NodeType::OUTPUT; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return false; }
	
	bool getGeometry(u16 output_idx, Geometry* result) override {
		return getInputGeometry(0, result);
	}

	void serialize(OutputMemoryStream& blob) const override {
		blob.write(user_channels_count);
	}

	void deserialize(InputMemoryStream& blob) override {
		blob.read(user_channels_count);
	}

	bool gui() override {
		ImGuiEx::NodeTitle("Output geometry");
		inputSlot();
		return ImGui::SliderInt("User channels", (i32*)&user_channels_count, 0, 4);
	}

	u32 user_channels_count = 0;
};

void ProceduralGeomGeneratorPlugin::apply() {	
	const Array<EntityRef>& selected = m_app.getWorldEditor().getSelectedEntities();
	if (selected.size() != 1) return;
	
	World* world = m_app.getWorldEditor().getWorld();
	bool children_removed = false;
	for (const Node* node : m_resource->m_nodes) {
		if (node->getType() == NodeType::INSTANTIATE_PREFAB) {
			if (!children_removed) {
				while (EntityPtr child = world->getFirstChild(selected[0])) {
					world->destroyEntity(*child);
				}
				children_removed = true;
			}
			const InstantiatePrefabNode* n = (const InstantiatePrefabNode*)node;
			n->instantiate(selected[0]);
		}
	}

	OutputGeometryNode* output = (OutputGeometryNode*)m_resource->m_nodes[0];
	Geometry geom(m_allocator);
	if (!output->getGeometry(0, &geom)) return;

	RenderScene* scene = (RenderScene*)world->getScene("renderer");
	
	if (!world->hasComponent(selected[0], PROCEDURAL_GEOM_TYPE)) world->createComponent(PROCEDURAL_GEOM_TYPE, selected[0]);

	Span<const u8> vertices((const u8*)geom.vertices.begin(), (u32)geom.vertices.byte_size());
	Span<const u8> indices((const u8*)geom.indices.begin(), (u32)geom.indices.byte_size());
	gpu::VertexDecl decl(geom.type);
	decl.addAttribute(0, 0, 3, gpu::AttributeType::FLOAT, 0);  // pos
	decl.addAttribute(1, 12, 2, gpu::AttributeType::FLOAT, 0); // uv
	decl.addAttribute(2, 20, 3, gpu::AttributeType::FLOAT, 0); // normal
	decl.addAttribute(3, 32, 3, gpu::AttributeType::FLOAT, 0); // tangent
	OutputMemoryStream user_vertices(m_allocator);
	if (output->user_channels_count > 0) {
		decl.addAttribute(4, 44, output->user_channels_count, gpu::AttributeType::U8, gpu::Attribute::NORMALIZED);
		user_vertices.reserve(geom.vertices.size() * (sizeof(Geometry::Vertex) + output->user_channels_count * sizeof(float))); 
		for (u32 i = 0; i < (u32)geom.vertices.size(); ++i) {
			user_vertices.write(geom.vertices[i]);
			const u8 v[] = { 0xff, 0, 0xff, 0xff };
			user_vertices.write(&v, sizeof(u8) * output->user_channels_count);
		}
		vertices = user_vertices;
	}
	scene->setProceduralGeometry(selected[0], vertices, decl, indices, gpu::DataType::U32);
	scene->setProceduralGeometryMaterial(selected[0], Path(m_resource->m_material));

}

Node* EditorResource::createNode(NodeType type, ImVec2 pos) {
	Node* node;
	switch (type) {
		case NodeType::CIRCLE: node = LUMIX_NEW(m_allocator, CircleNode); break;
		case NodeType::CONE: node = LUMIX_NEW(m_allocator, ConeNode); break;
		case NodeType::CUBE: node = LUMIX_NEW(m_allocator, CubeNode); break;
		case NodeType::CYLINDER: node = LUMIX_NEW(m_allocator, CylinderNode); break;
		case NodeType::DISTRIBUTE_POINT_ON_FACES: node = LUMIX_NEW(m_allocator, DistributePointsOnFacesNode); break;
		case NodeType::EXTRUDE_ALONG: node = LUMIX_NEW(m_allocator, ExtrudeAlongNode); break;
		case NodeType::GRID: node = LUMIX_NEW(m_allocator, GridNode); break;
		case NodeType::INSTANTIATE_PREFAB: node = LUMIX_NEW(m_allocator, InstantiatePrefabNode); break;
		case NodeType::LINE: node = LUMIX_NEW(m_allocator, LineNode); break;
		case NodeType::MERGE: node = LUMIX_NEW(m_allocator, MergeNode); break;
		case NodeType::MODEL: node = LUMIX_NEW(m_allocator, ModelNode); break;
		case NodeType::OUTPUT: node = LUMIX_NEW(m_allocator, OutputGeometryNode); break;
		case NodeType::POINT: node = LUMIX_NEW(m_allocator, PointNode); break;
		case NodeType::PLACE_INSTANCES_AT_POINTS: node = LUMIX_NEW(m_allocator, PlaceInstancesAtPoints); break;
		case NodeType::SCALE: node = LUMIX_NEW(m_allocator, ScaleNode); break;
		case NodeType::SPHERE: node = LUMIX_NEW(m_allocator, SphereNode); break;
		case NodeType::SPLINE: node = LUMIX_NEW(m_allocator, SplineNode); break;
		case NodeType::SPIRAL: node = LUMIX_NEW(m_allocator, SpiralNode); break;
		case NodeType::TRANSFORM: node = LUMIX_NEW(m_allocator, TransformNode); break;
		default: ASSERT(false); return nullptr;
	}
	node->m_pos = pos;
	node->m_resource = this;
	node->m_allocator = &m_allocator;
	node->m_id = ++m_node_id_genereator;
	m_nodes.push(node);
	return node;
}

Node* ProceduralGeomGeneratorPlugin::addNode(NodeType type, ImVec2 pos, bool save_undo) {
	Node* n = m_resource->createNode(type, pos);
	n->m_pos = pos;
	if (save_undo) pushUndo(NO_MERGE_UNDO);
	return n;
}

LUMIX_STUDIO_ENTRY(procedural_geom) {
	auto* plugin = LUMIX_NEW(app.getAllocator(), ProceduralGeomGeneratorPlugin)(app);
	app.addPlugin(*plugin);
	return nullptr;
}

} // namespace Lumix

#define LUMIX_NO_CUSTOM_CRT
#include <string.h>
#include "core/allocator.h"
#include "core/array.h"
#include "core/crt.h"
#include "core/log.h"
#include "core/math.h"
#include "core/os.h"
#include "core/profiler.h"
#include "core/string.h"
#include "editor/asset_browser.h"
#include "editor/prefab_system.h"
#include "editor/settings.h"
#include "editor/studio_app.h"
#include "editor/utils.h"
#include "editor/world_editor.h"
#include "engine/core.h"
#include "engine/engine.h"
#include "engine/prefab.h"
#include "engine/resource_manager.h"
#include "engine/world.h"
#include "renderer/material.h"
#include "renderer/model.h"
#include "renderer/render_module.h"
#include "renderer/terrain.h"
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
	SCALE,
	ROTATE_POINTS,

	INDEX,
	FLOAT_CONST,
	MATH,

	SET_POSITION,
	SPLIT_VEC3,
	MAKE_VEC3,
	POSITION,
	RANDOM,
	NOISE,
	CURVE,
	MIX,
	MAP_RANGE,
	SET_NORMAL,
	PIN,
	NORMAL,
	SNAP_TO_TERRAIN
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

	Array<Vertex> vertices;
	Array<u32> indices;
	gpu::PrimitiveType type;
	Transform to_world = Transform::IDENTITY;
};

struct Input;
struct EditorResource;

struct Node : NodeEditorNode {
	struct Context {
		u32 index;
		const Geometry::Vertex* vertex = nullptr;
		Geometry* geometry = nullptr;
	};

	struct Result {
		static Result geom() { Result r(0); r.type = GEOMETRY; return r; }

		Result() {}

		Result(bool) = delete;

		Result(u32 value) {
			type = I32;
			i32_value = value;
		}

		Result(i32 value) {
			type = I32;
			i32_value = value;
		}

		Result(float value) {
			type = FLOAT;
			f_value = value;
		}

		Result(Vec3 value) {
			type = VEC3;
			vec3_value = value;
		}

		enum Type {
			INVALID,
			FLOAT,
			I32,
			VEC3,
			GEOMETRY
		};

		Type type = INVALID;
		union {
			float f_value;
			i32 i32_value;
			Vec3 vec3_value;
			// Type::GEOMETRY result is passed in Context::geometry to limit number allocations
		};

		bool isValid() const { return type != INVALID; }
	};

	Node(EditorResource& resource);
	
	virtual Result eval(u16 output_idx, const Context& ctx) = 0;
	virtual bool gui() = 0;
	virtual NodeType getType() const = 0;
	virtual void serialize(OutputMemoryStream& blob) const {}
	virtual void deserialize(InputMemoryStream& blob) {}

	Input getInput(u16 input) const;
	Result getInputResult(u16 input, const Context& ctx) const;
	bool getInputGeometry(u16 input, Geometry& geom) const;
	Result error(const char* msg);

	bool nodeGUI() override {
		ImGuiEx::BeginNode(m_id, m_pos, &m_selected);
		m_input_counter = 0;
		m_output_counter = 0;
		bool res = gui();
		
		if (m_error.length() > 0) {
			ImGui::PushStyleColor(ImGuiCol_Border, IM_COL32(0xff, 0, 0, 0xff));
		}
		else if (!m_reachable) {
			ImGui::PushStyleColor(ImGuiCol_Border, ImGui::GetColorU32(ImGuiCol_TitleBg));
		}
		ImGuiEx::EndNode();
		if (m_error.length() > 0) {
			ImGui::PopStyleColor();
			if (ImGui::IsItemHovered()) ImGui::SetTooltip("%s", m_error.c_str());
		}
		else if (!m_reachable) {
			ImGui::PopStyleColor();
		}
		
		return res;
	}

	static ImGuiEx::PinShape toShape(Result::Type type) {
		switch(type) {
			case Result::INVALID: return ImGuiEx::PinShape::CIRCLE;
			case Result::FLOAT: return ImGuiEx::PinShape::SQUARE;
			case Result::I32: return ImGuiEx::PinShape::SQUARE;
			case Result::VEC3: return ImGuiEx::PinShape::SQUARE;
			case Result::GEOMETRY: return ImGuiEx::PinShape::TRIANGLE;
		}
		ASSERT(false);
		return ImGuiEx::PinShape::CIRCLE;
	}

	void inputSlot(Result::Type type = Result::Type::INVALID) {
		ImGuiEx::Pin(m_id | ((u32)m_input_counter << 16), true, toShape(type));
		++m_input_counter;
	}

	void outputSlot(Result::Type type = Result::Type::INVALID) {
		ImGuiEx::Pin(m_id | ((u32)m_output_counter << 16) | OUTPUT_FLAG, false, toShape(type));
		++m_output_counter;
	}

	EditorResource& m_resource;
	IAllocator& m_allocator;
	bool m_selected = false;
	bool m_reachable = false;
	u16 m_input_counter = 0;
	u16 m_output_counter = 0;
	String m_error;
};

struct Input {
	Node* node = nullptr;
	u16 output_idx;
	[[nodiscard]] Node::Result eval(const Node::Context& ctx) const {
		return node ? node->eval(output_idx, ctx) : Node::Result();
	}
	operator bool() const { return node != nullptr; }
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
		// m_nodes[0] is output, it can not be deleted
		for (i32 i = m_nodes.size() - 1; i > 0; --i) {
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

	void markReachable(Node& n);

	void deleteUnreachable() {
		markReachableNodes();
		for (i32 i = m_nodes.size() - 1; i > 0; --i) {
			Node* node = m_nodes[i];
			if (!node->m_reachable) {
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

	void markReachableNodes() {
		for (Node* n : m_nodes) n->m_reachable = false;
		markReachable(*m_nodes[0]);
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
	{ 0, "Curve", NodeType::CURVE },
	{ 0,   "Cylinder", NodeType::CYLINDER },
	{ 'D', "Distribute points on faces", NodeType::DISTRIBUTE_POINT_ON_FACES },
	{ 'E', "Extrude along", NodeType::EXTRUDE_ALONG },
	{ '1', "Float constant", NodeType::FLOAT_CONST },
	{ 'G', "Grid", NodeType::GRID },
	{ 0, "Instantiate prefab", NodeType::INSTANTIATE_PREFAB },
	{ 'I', "Index", NodeType::INDEX },
	{ 'L', "Line", NodeType::LINE },
	{ '3', "Make vector3", NodeType::MAKE_VEC3 },
	{ 0, "Map range", NodeType::MAP_RANGE },
	{ 'A', "Merge", NodeType::MERGE },
	{ 'M', "Math", NodeType::MATH },
	{ 'X', "Mix", NodeType::MIX },
	{ 0, "Model", NodeType::MODEL },
	{ 'N', "Noise", NodeType::NOISE },
	{ 0, "Normal", NodeType::NORMAL },
	{ 0, "Random", NodeType::RANDOM },
	{ 'I', "Place instances at points", NodeType::PLACE_INSTANCES_AT_POINTS },
	{ 0, "Point", NodeType::POINT },
	{ 'P', "Position", NodeType::POSITION },
	{ 'R', "Rotate points", NodeType::ROTATE_POINTS },
	{ 0, "Scale", NodeType::SCALE },
	{ 0, "Set normal", NodeType::SET_NORMAL },
	{ 0, "Set position", NodeType::SET_POSITION },
	{ 0, "Snap to terrain", NodeType::SNAP_TO_TERRAIN },
	{ 0, "Sphere", NodeType::SPHERE },
	{ 'Q', "Spiral", NodeType::SPIRAL},
	{ 'S', "Spline", NodeType::SPLINE },
	{ 'V', "Split vector3", NodeType::SPLIT_VEC3 },
	{ 'T', "Transform", NodeType::TRANSFORM },
};

struct CurveNode : Node {
	CurveNode(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::CURVE; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }

	void serialize(OutputMemoryStream& blob) const override {
		blob.write(m_point_count);
		blob.write(m_points, sizeof(m_points[0]) * m_point_count);
	}

	void deserialize(InputMemoryStream& blob) override {
		blob.read(m_point_count);
		blob.read(m_points, sizeof(m_points[0]) * m_point_count);
	}

	static float cubicInterpolate(float y0, float y1, float y2, float y3, float t) {
		#if 0 // cubic
			float a0 = y3 - y2 - y0 + y1;
			float a1 = y0 - y1 - a0;
			float a2 = y2 - y0;
			float a3 = y1;
		#else // catmull rom
			float a0 = -0.5f * y0 + 1.5f * y1 - 1.5f * y2 + 0.5f * y3;
			float a1 = y0 - 2.5f * y1 + 2.f * y2 - 0.5f * y3;
			float a2 = -0.5f * y0 + 0.5f * y2;
			float a3 = y1;
		#endif

		float t2 = t * t;
		return a0 * t * t2 + a1 * t2 + a2 * t + a3;
	}

	float evalCurve(float x) const {
		auto p = [&](i32 idx){
			if (idx < 0) return m_points[0].y - (m_points[1].y - m_points[0].y);
			if (idx >= (i32)m_point_count) return m_points[m_point_count - 1].y - (m_points[m_point_count - 2].y - m_points[m_point_count - 1].y);
			return m_points[idx].y;
		};
		for (i32 j = 1; j < (i32)m_point_count; ++j) {
			if (m_points[j].x >= x) {
				const float t = (x - m_points[j - 1].x) / (m_points[j].x - m_points[j - 1].x);
				return cubicInterpolate(p(j - 2), p(j - 1), p(j), p(j + 1), t);
			}
		}
		return m_points[0].y;
	}
	
	static float len(ImVec2 p) {
		return sqrtf(p.x * p.x + p.y * p.y);
	}

	static ImVec2 mix(ImVec2 a, ImVec2 b, ImVec2 t) {
		return {
			a.x * (1 - t.x) + b.x * t.x,
			a.y * (1 - t.y) + b.y * t.y
		};
	}

	bool curve() {
		const ImU32 color_border = ImGui::GetColorU32(ImGuiCol_Border);
		const ImU32 color = ImGui::GetColorU32(ImGuiCol_PlotLines);
		const ImU32 color_hovered = ImGui::GetColorU32(ImGuiCol_PlotLinesHovered);
		ImGui::InvisibleButton("curve", ImVec2(210, 210));
		const bool is_hovered = ImGui::IsItemHovered();
		ImVec2 mp = ImGui::GetMousePos();
		ImDrawList* dl = ImGui::GetWindowDrawList();
		
		ImVec2 from = ImGui::GetItemRectMin() + ImVec2(5, 5);
		ImVec2 to = from + ImGui::GetItemRectSize() - ImVec2(10, 10);
		swap(to.y, from.y);

		ImVec2 prev_p = m_points[0];
		for (u32 i = 1; i < 51; ++i) {
			float x = i / float(50);
			float y = clamp(evalCurve(x), 0.f, 1.f);
			ImVec2 p(x, y);
			ImVec2 a = mix(from, to, prev_p);
			ImVec2 b = mix(from, to, p);
			dl->AddLine(a, b, color);
			prev_p = p;
		}
		dl->AddRect(from - ImVec2(5, -5), to + ImVec2(5, -5), color_border);

		i32 hovered_point = -1;
		bool changed = false;
		if (ImGui::IsMouseReleased(0)) m_dragged_point = -1;
		for (u32 i = 0; i < m_point_count; ++i) {
			ImVec2 center = mix(from, to, m_points[i]);
			const bool is_point_hovered = is_hovered && len(mp - center) < 5;
			if (is_point_hovered) hovered_point = i;
			dl->AddCircle(center, 5, is_point_hovered ? color_hovered : color);
			if (is_point_hovered && ImGui::IsMouseClicked(0)) m_dragged_point = i;
			if (ImGui::IsMouseDragging(0) && m_dragged_point == i) {
				m_points[i] = m_points[i] + ImGui::GetMouseDragDelta() / (to - from);
				changed = true;
				if (i > 0 && m_points[i].x < m_points[i - 1].x) {
					swap(m_points[i], m_points[i - 1]);
					--m_dragged_point;
				}
				if (i < m_point_count - 1 && m_points[i].x > m_points[i + 1].x) {
					++m_dragged_point;
					swap(m_points[i], m_points[i + 1]);
				}
				ImGui::ResetMouseDragDelta();
			}
		}

		if (is_hovered && ImGui::IsMouseDoubleClicked(0)) {
			if (hovered_point >= 0) {
				if (m_point_count > 2) {
					memmove(m_points + hovered_point, m_points + hovered_point + 1, (m_point_count - hovered_point - 1) * sizeof(m_points[0]));
					--m_point_count;
					changed = true;
				}
			}
			else if (m_point_count < lengthOf(m_points)) {
				ImVec2 t = (mp - from) / (to - from);
				for (u32 i = 0; i < m_point_count; ++i) {
					if (t.x < m_points[i].x) {
						memmove(m_points + i + 1, m_points + i, (m_point_count - i) * sizeof(m_points[0]));
						m_points[i] = t;
						++m_point_count;
						changed = true;
						break;
					}
				}
			}
		}

		m_points[0].x = 0;
		m_points[m_point_count - 1].x = 1;
		for (u32 i = 0; i < m_point_count; ++i) {
			m_points[i].y = clamp(m_points[i].y, 0.f, 1.f);
		}
		return changed;
	}

	Result eval(u16 output_idx, const Context& ctx) override {
		const Result val = getInputResult(0, ctx);
		
		switch (val.type) {
			case Result::GEOMETRY:
			case Result::INVALID:
				return error("Invalid input");
			case Result::I32:
				return evalCurve((float)val.i32_value);
			case Result::FLOAT:
				return evalCurve(val.f_value);
			case Result::VEC3:
				return Vec3(evalCurve(val.vec3_value.x), evalCurve(val.vec3_value.y), evalCurve(val.vec3_value.z));
		}
		ASSERT(false);
		return {};
	}
	
	bool gui() override {
		ImGuiEx::NodeTitle("Curve");
		inputSlot();
		outputSlot();
		return curve();
	}

	ImVec2 m_points[16] = { ImVec2(0, 0), ImVec2(1, 1) };
	u32 m_point_count = 2;
	i32 m_dragged_point = -1;
};

struct NormalNode : Node {
	NormalNode(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::NORMAL; }
	bool hasInputPins() const override { return false; }
	bool hasOutputPins() const override { return true; }
	void serialize(OutputMemoryStream& blob) const override {}
	void deserialize(InputMemoryStream& blob) override {}
	
	Result eval(u16 output_idx, const Context& ctx) override {
		return ctx.vertex ? Result(ctx.vertex->normal) : error("Invalid context");
	}
	
	bool gui() override {
		ImGuiEx::NodeTitle("Normal");
		outputSlot(Result::VEC3);
		ImGui::TextUnformatted(" ");
		return false;
	}
};
struct PositionNode : Node {
	PositionNode(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::POSITION; }
	bool hasInputPins() const override { return false; }
	bool hasOutputPins() const override { return true; }
	void serialize(OutputMemoryStream& blob) const override {}
	void deserialize(InputMemoryStream& blob) override {}
	
	Result eval(u16 output_idx, const Context& ctx) override {
		return ctx.vertex ? Result(ctx.vertex->position) : error("Invalid context");
	}
	
	bool gui() override {
		ImGuiEx::NodeTitle("Position");
		outputSlot(Result::VEC3);
		ImGui::TextUnformatted(" ");
		return false;
	}
};

struct IndexNode : Node {
	IndexNode(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::INDEX; }
	bool hasInputPins() const override { return false; }
	bool hasOutputPins() const override { return true; }
	void serialize(OutputMemoryStream& blob) const override {}
	void deserialize(InputMemoryStream& blob) override {}
	
	Result eval(u16 output_idx, const Context& ctx) override {
		return Result(ctx.index);
	}
	
	bool gui() override {
		ImGuiEx::NodeTitle("Index");
		outputSlot(Result::I32);
		ImGui::TextUnformatted(" ");
		return false;
	}
};

struct CircleNode : Node {
	CircleNode(EditorResource& resource) : Node(resource) {}
	
	NodeType getType() const override { return NodeType::CIRCLE; }

	bool hasInputPins() const override { return false; }
	bool hasOutputPins() const override { return true; }

	bool gui() override {
		ImGuiEx::NodeTitle("Circle");
		outputSlot(Result::GEOMETRY);
		bool res = ImGui::DragFloat("Radius", &radius);
		res = ImGui::DragInt("Subdivision", (i32*)&subdivision) || res;
		return res;
	}

	Result eval(u16 output_idx, const Node::Context& ctx) override {
		if (!ctx.geometry) return error("Invalid context");
		
		ctx.geometry->vertices.reserve(subdivision + 1);
		for (u32 i = 0; i <= subdivision; ++i) {
			const float a = i / (float)subdivision * PI * 2;
			Geometry::Vertex& v = ctx.geometry->vertices.emplace();
			v.position = Vec3(cosf(a) * radius, sinf(a) * radius, 0);
			v.normal = normalize(v.position);
			v.tangent = Vec3(-v.position.y, v.position.x, 0);
			v.uv = Vec2(i / (float)subdivision, 0);
		}
		ctx.geometry->type = gpu::PrimitiveType::LINES;
		return Result::geom();
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
	LineNode(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::LINE; }

	bool hasInputPins() const override { return false; }
	bool hasOutputPins() const override { return true; }

	bool gui() override {
		ImGuiEx::NodeTitle("Line");
		outputSlot(Result::GEOMETRY);
		bool res = ImGui::DragFloat("Size", &size);
		res = ImGui::DragInt("Subdivision", (i32*)&subdivision) || res;
		return res;
	}

	Result eval(u16 output_idx, const Context& ctx) override {
		if (!ctx.geometry) return error("Invalid context");

		ctx.geometry->vertices.reserve(subdivision + 1);
		for (u32 i = 0; i <= subdivision; ++i) {
			Geometry::Vertex& v = ctx.geometry->vertices.emplace();
			v.position = Vec3(-size * (i / (float)subdivision * 2 - 1), 0, 0);
			v.uv = Vec2(i / (float)subdivision, 0);
			v.normal = Vec3(0, 1, 0);
			v.tangent = Vec3(1, 0, 0);
		}
		ctx.geometry->type = gpu::PrimitiveType::LINES;
		return Result::geom();
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
	PlaceInstancesAtPoints(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::PLACE_INSTANCES_AT_POINTS; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }

	bool gui() override {
		ImGuiEx::NodeTitle("Place instances at points");
		ImGui::BeginGroup();
		inputSlot(Result::GEOMETRY); ImGui::TextUnformatted("Points");
		inputSlot(Result::GEOMETRY); ImGui::TextUnformatted("Instance");
		ImGui::EndGroup();
		ImGui::SameLine();
		outputSlot(Result::GEOMETRY);
		return false;
	}

	Result eval(u16 output_idx, const Context& ctx) override {
		if (!ctx.geometry) return error("Invalid context");

		Geometry points(m_allocator);
		Geometry instance(m_allocator);

		if (!getInputGeometry(0, points)) return error("Invalid inputs");
		if (!getInputGeometry(1, instance)) return error("Invalid inputs");
		if (instance.type != gpu::PrimitiveType::TRIANGLES) return error("Instance is not a mesh");

		ctx.geometry->vertices.reserve(points.vertices.size() * instance.vertices.size());
		ctx.geometry->indices.reserve(points.indices.size() * instance.indices.size());

		for (const Geometry::Vertex& point : points.vertices) {
			Matrix mtx;
			const Vec3 dir = cross(point.normal, point.tangent);
			mtx.lookAt(point.position, point.position + dir, point.normal);
			mtx = mtx.fastInverted();

			const u32 index_offset = ctx.geometry->vertices.size();

			for (const Geometry::Vertex& src_v : instance.vertices) {
				Geometry::Vertex& v = ctx.geometry->vertices.emplace();
				v = src_v;
				v.position = mtx.transformPoint(v.position);
				v.tangent = mtx.transformVector(v.tangent);
				v.normal = mtx.transformVector(v.normal);
			}

			for (u32 idx : instance.indices) {
				ctx.geometry->indices.push(index_offset + idx);
			}
		}
		
		ctx.geometry->type = gpu::PrimitiveType::TRIANGLES;
		ctx.geometry->to_world = points.to_world;
		return Result::geom();
	}
};

struct ModelNode : Node {
	ModelNode(EditorResource& resource) : Node(resource) {}

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
			ResourceManagerHub& rm = m_resource.m_app.getEngine().getResourceManager();
			m_model = rm.load<Model>(Path(path));
		}
		else {
			m_model = nullptr;
		}
	}
	
	bool gui() override {
		ImGuiEx::NodeTitle("Model");
		outputSlot(Result::GEOMETRY);
		Path path = m_model ? m_model->getPath() : Path();
		if (m_resource.m_app.getAssetBrowser().resourceInput("Asset", path, Model::TYPE, 150)) {
			if (m_model) m_model->decRefCount();
			ResourceManagerHub& rm = m_resource.m_app.getEngine().getResourceManager();
			m_model = rm.load<Model>(path);
			return true;
		}
		return false;
	}

	Result eval(u16 output_idx, const Context& ctx) override {
		if (!ctx.geometry) return error("Invalid context");

		if (!m_model) return error("Model is not set");
		if (!m_model->isReady()) return error("Model is not ready");;

		for (u32 i = 0, ci= m_model->getMeshCount(); i < ci; ++i) {
			const Mesh& mesh = m_model->getMesh(i);
			
			const u32 indices_offset = ctx.geometry->vertices.size();

			for (const Vec3& vin : mesh.vertices) {
				Geometry::Vertex& vout = ctx.geometry->vertices.emplace();
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
				ctx.geometry->indices.push(idx + indices_offset);
			}
		}

		ctx.geometry->type = gpu::PrimitiveType::TRIANGLES;
		return Result::geom();
	}

	Model* m_model = nullptr;
};

struct InstantiatePrefabNode : Node {
	InstantiatePrefabNode(EditorResource& resource) : Node(resource) {}

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
			ResourceManagerHub& rm = m_resource.m_app.getEngine().getResourceManager();
			m_prefab = rm.load<PrefabResource>(Path(path));
		}
		else {
			m_prefab = nullptr;
		}
	}

	bool gui() override {
		ImGuiEx::NodeTitle("Instantiate prefab");
		inputSlot(Result::GEOMETRY);
		ImGui::TextUnformatted("Points");
		Path path = m_prefab ? m_prefab->getPath() : Path();
		ResourceManagerHub& rm = m_resource.m_app.getEngine().getResourceManager();
		if (m_resource.m_app.getAssetBrowser().resourceInput("Prefab", path, PrefabResource::TYPE, 150)) {
			if (m_prefab) m_prefab->decRefCount();
			m_prefab = rm.load<PrefabResource>(path);
			return true;
		}
		if (ImGui::Button("Instantiate")) {
			WorldEditor& editor = m_resource.m_app.getWorldEditor();
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
		
		WorldEditor& editor = m_resource.m_app.getWorldEditor();
		World& world = *editor.getWorld();
		
		const Transform& transform = world.getTransform(parent);
		Geometry points(m_allocator);
		if (!getInputGeometry(0, points)) {
			logError("Invalid input");
			return;
		}

		PrefabSystem& prefab_system = m_resource.m_app.getWorldEditor().getPrefabSystem();
		for (const Geometry::Vertex& v : points.vertices) {
			const DVec3 p = transform.transform(v.position);
			// TODO rotation
			const EntityPtr e = prefab_system.instantiatePrefab(*m_prefab, p, transform.rot, transform.scale);
			// TODO editor command
			if (e.isValid()) world.setParent(parent, *e);
		}
	}

	Result eval(u16 output_idx, const Context& ctx) override {
		return {};
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
	PointNode(EditorResource& resource) : Node(resource) {}

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
		outputSlot(Result::GEOMETRY);
		bool res = ImGui::DragFloat("X", &position.x);
		res = ImGui::DragFloat("Y", &position.y) || res;
		res = ImGui::DragFloat("Z", &position.z) || res;
		return res;
	}

	Result eval(u16 output_idx, const Context& ctx) override {
		if (!ctx.geometry) return error("Invalid context");

		Geometry::Vertex& v = ctx.geometry->vertices.emplace();
		v.position = position;
		v.normal = Vec3(0, 1, 0);
		v.tangent = Vec3(1, 0, 0);
		v.uv = Vec2(0, 0);
		ctx.geometry->type = gpu::PrimitiveType::POINTS;
		return Result::geom();
	}

	Vec3 position = Vec3(0, 0, 0);
};

struct SpiralNode : Node {
	SpiralNode(EditorResource& resource) : Node(resource) {}

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
		outputSlot(Result::GEOMETRY);
		bool res = ImGui::DragFloat("Height", &height, 1, FLT_MIN, FLT_MAX);
		res = ImGui::DragFloat("Start radius", &radius_start, 1, FLT_MIN, FLT_MAX) || res;
		res = ImGui::DragFloat("End radius", &radius_end, 1, FLT_MIN, FLT_MAX) || res;
		res = ImGui::DragFloat("Turns", &turns, 1, FLT_MIN, FLT_MAX) || res;
		res = ImGui::DragInt("Resolution", (i32*)&resolution, 1, 1, 999999) || res;
		return res;
	}
	
	Result eval(u16 output_idx, const Context& ctx) override {
		if (!ctx.geometry) return error("Invalid context");

		const u32 count = u32(turns * resolution + 0.5f);
		if (count == 0) return error("0 points generated");
		ctx.geometry->vertices.reserve(count);
		
		for (u32 i = 0; i < count; ++i) {
			const float t = i / float(count - 1);
			const float radius = radius_start * (1 - t) + radius_end * t; 
			const float h = height * t;
			const float angle = (turns * t) * 2 * PI;
			Geometry::Vertex& v = ctx.geometry->vertices.emplace();
			v.position = Vec3(cosf(angle) * radius, sinf(angle) * radius, h);
			v.uv = Vec2(h, 0);
			v.normal = normalize(Vec3(v.position.x, v.position.y, 0));
			v.tangent = Vec3(-v.normal.y, v.normal.x, 0);
		}
		ctx.geometry->type = gpu::PrimitiveType::POINTS;
		return Result::geom();
	}

	float height = 1;
	float radius_start = 1;
	float radius_end = 1;
	float turns = 4;
	u32 resolution = 16;
};

struct SplineNode : Node {
	SplineNode(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::SPLINE; }

	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }
	
	bool gui() override {
		ImGuiEx::NodeTitle("Spline");
		outputSlot(Result::GEOMETRY);
		return ImGui::DragFloat("Step", &step);
	}

	Result eval(u16 output_idx, const Context& ctx) override {
		if (!ctx.geometry) return error("Invalid context");

		WorldEditor& editor = m_resource.m_app.getWorldEditor();
		const Array<EntityRef>& selected = editor.getSelectedEntities();
		if (selected.size() != 1) return error("Exactly one entity must be selected");

		World& world = *editor.getWorld();
		if (!world.hasComponent(selected[0], SPLINE_TYPE)) return error("Selected entity does not have spline component");

		CoreModule* core_module = (CoreModule*)world.getModule(SPLINE_TYPE);
		const Spline& spline = core_module->getSpline(selected[0]);
		if (spline.points.empty()) return error("Spline is empty");

		SplineIterator iterator(spline.points);
		float d = 0;
		ctx.geometry->vertices.reserve(512);
		Vec3 prev_p = spline.points[0];
		while (!iterator.isEnd()) {
			const Vec3 dir = normalize(iterator.getDir());
			Geometry::Vertex& v = ctx.geometry->vertices.emplace();
			v.position = iterator.getPosition();
			v.normal = iterator.getNormal();
			v.tangent = dir;
			v.uv = Vec2(0, d);
			// TODO actual step is not uniform
			iterator.move(step);
			d += length(v.position - prev_p);
			prev_p = v.position;
		}
		ctx.geometry->type = gpu::PrimitiveType::POINTS;
		ctx.geometry->to_world = world.getTransform(selected[0]);
		return Result::geom();
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
	ExtrudeAlongNode(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::EXTRUDE_ALONG; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }

	bool gui() override {
		ImGuiEx::NodeTitle("Extrude along");
		ImGui::BeginGroup();
		inputSlot(Result::GEOMETRY); ImGui::TextUnformatted("Profile");
		inputSlot(Result::GEOMETRY); ImGui::TextUnformatted("Path");
		ImGui::EndGroup();
		ImGui::SameLine();
		outputSlot(Result::GEOMETRY);
		return false;
	}

	Result eval(u16 output_idx, const Context& ctx) override {
		if (!ctx.geometry) return error("Invalid context");

		Geometry profile(m_allocator);
		Geometry path(m_allocator);

		if (!getInputGeometry(0, profile)) return error("Invalid input");
		if (!getInputGeometry(1, path)) return error("Invalid input");
		if (path.type != gpu::PrimitiveType::LINES && path.type != gpu::PrimitiveType::POINTS) return error("Invalid path");
		
		switch (profile.type) {
			case gpu::PrimitiveType::POINTS:
			case gpu::PrimitiveType::LINES: {
				ctx.geometry->vertices.reserve(path.vertices.size() * profile.vertices.size());
				
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
						Geometry::Vertex& v = ctx.geometry->vertices.emplace();
						v.position = p + side * profile.vertices[i].position.x + up * profile.vertices[i].position.y;
						v.uv = profile.vertices[i].uv;
						v.uv.y = d;
						v.normal = profile.vertices[i].normal;
						v.normal = side * v.normal.x + up * v.normal.y + dir * v.normal.z;
						v.tangent = profile.vertices[i].tangent;
						v.tangent = side * v.tangent.x + up * v.tangent.y + dir * v.tangent.z;
					}
		
					if (rows > 1) {
						const u32 offset = ctx.geometry->vertices.size() - 2 * w;
						for (u32 i = 0; i < w - 1; ++i) {
							ctx.geometry->indices.push(offset + i);
							ctx.geometry->indices.push(offset + i + 1);
							ctx.geometry->indices.push(offset + i + w);
		
							ctx.geometry->indices.push(offset + i + w + 1);
							ctx.geometry->indices.push(offset + i + w);
							ctx.geometry->indices.push(offset + i + 1);
						}
					}
					prev_p = p;
				}
		
				ctx.geometry->type = gpu::PrimitiveType::TRIANGLES;
				ctx.geometry->to_world = path.to_world;
				return Result::geom();
			}
			default: 
				return error("Invalid profile");
		}
	}
};

struct CylinderNode : Node {
	CylinderNode(EditorResource& resource) : Node(resource) {}

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
	
	Result eval(u16 output_idx, const Context& ctx) override {
		if (!ctx.geometry) return error("Invalid context");

		ctx.geometry->vertices.reserve(2 * subdivision + 4);

		{
			Geometry::Vertex& v1 = ctx.geometry->vertices.emplace();
			v1.position = Vec3(0, 0, 0.5f * height);
			v1.normal = Vec3(0, 0, 1);
			v1.tangent = Vec3(1, 0, 0);
			v1.uv = Vec2(0, 0);
	
			Geometry::Vertex& v2 = ctx.geometry->vertices.emplace();
			v2.position = Vec3(0, 0, -0.5f * height);
			v2.normal = Vec3(0, 0, -1);
			v2.tangent = Vec3(-1, 0, 0);
			v2.uv = Vec2(0, 0);
		}

		for (u32 i = 0; i < subdivision; ++i) {
			const float a = i / float(subdivision) * PI * 2;
			Geometry::Vertex& v = ctx.geometry->vertices.emplace();
			v.position = Vec3(cosf(a) * radius, sinf(a) * radius, 0.5f * height);
			v.normal = Vec3(0, 0, 1);
			v.tangent = Vec3(1, 0, 0);
			v.uv = Vec2(v.position.x, v.position.y);
		}

		for (u32 i = 0; i < subdivision; ++i) {
			const float a = i / float(subdivision) * PI * 2;
			Geometry::Vertex& v = ctx.geometry->vertices.emplace();
			v.position = Vec3(cosf(a) * radius, sinf(a) * radius, -0.5f * height);
			v.normal = Vec3(0, 0, -1);
			v.tangent = Vec3(-1, 0, 0);
			v.uv = Vec2(v.position.x, v.position.y);
		}

		for (u32 i = 0; i < subdivision; ++i) {
			const float a = i / float(subdivision) * PI * 2;
			Geometry::Vertex& v = ctx.geometry->vertices.emplace();
			v.position = Vec3(cosf(a) * radius, sinf(a) * radius, 0.5f * height);
			v.normal = normalize(Vec3(v.position.x, v.position.y, 0));
			v.tangent = Vec3(-1, 0, 0);
			v.uv = Vec2(v.position.x, v.position.y);

			Geometry::Vertex& v2 = ctx.geometry->vertices.emplace();
			v2.position = Vec3(cosf(a) * radius, sinf(a) * radius, -0.5f * height);
			v2.normal = normalize(Vec3(v2.position.x, v2.position.y, 0));
			v2.tangent = Vec3(-1, 0, 0);
			v2.uv = Vec2(v2.position.x, v2.position.y);
		}


		ctx.geometry->indices.reserve(subdivision * 3 * 2);
		for (u32 i = 0; i < subdivision; ++i) {
			ctx.geometry->indices.push(0);
			ctx.geometry->indices.push(2 + i);
			ctx.geometry->indices.push(2 + (i + 1) % subdivision);
		}

		for (u32 i = 0; i < subdivision; ++i) {
			ctx.geometry->indices.push(1);
			ctx.geometry->indices.push(2 + subdivision + (i + 1) % subdivision);
			ctx.geometry->indices.push(2 + subdivision + i);
		}

		for (u32 i = 0; i < subdivision; ++i) {
			ctx.geometry->indices.push(2 + 2 * subdivision + 2 * i);
			ctx.geometry->indices.push(2 + 2 * subdivision + ((2 * i) + 1) % (2 * subdivision));
			ctx.geometry->indices.push(2 + 2 * subdivision + ((2 * i) + 2) % (2 * subdivision));

			ctx.geometry->indices.push(2 + 2 * subdivision + ((2 * i) + 2) % (2 * subdivision));
			ctx.geometry->indices.push(2 + 2 * subdivision + ((2 * i) + 1) % (2 * subdivision));
			ctx.geometry->indices.push(2 + 2 * subdivision + ((2 * i) + 3) % (2 * subdivision));
		}

		ctx.geometry->type = gpu::PrimitiveType::TRIANGLES;
		return Result::geom();
	};

	bool gui() override {
		ImGuiEx::NodeTitle("Cylinder");
		outputSlot(Result::GEOMETRY);
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
	ConeNode(EditorResource& resource) : Node(resource) {}
	
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
	
	Result eval(u16 output_idx, const Context& ctx) override {
		if (!ctx.geometry) return error("Invalid context");

		ctx.geometry->vertices.reserve(2 * subdivision + 2);
		ctx.geometry->indices.reserve(subdivision * 6);

		for (u32 i = 0; i < subdivision; ++i) {
			const float angle = i / float(subdivision) * 2 * PI;
			Geometry::Vertex& vertex = ctx.geometry->vertices.emplace();
			vertex.position = Vec3(cosf(angle), sinf(angle), 0) * base_size;
			vertex.uv = Vec2((float)i, 0);
			vertex.normal = normalize(vertex.position);
			vertex.tangent = Vec3(vertex.normal.y, -vertex.normal.x, 0);
		}

		{
			Geometry::Vertex& vertex = ctx.geometry->vertices.emplace();
			vertex.position = Vec3(0, 0, height);
			vertex.normal = Vec3(0, 0, 1);
			vertex.tangent = Vec3(1, 0, 0);
			vertex.uv = Vec2(0, 1);
		}

		for (u32 i = 0; i < subdivision; ++i) {
			const float angle = i / float(subdivision) * 2 * PI;
			Geometry::Vertex& vertex = ctx.geometry->vertices.emplace();
			vertex.position = Vec3(cosf(angle), sinf(angle), 0) * base_size;
			vertex.uv = Vec2((float)i, 0);
			vertex.normal = Vec3(0, 0, -1);
			vertex.tangent = Vec3(1, 0, 0);
		}

		{
			Geometry::Vertex& vertex = ctx.geometry->vertices.emplace();
			vertex.position = Vec3(0, 0, 0);
			vertex.normal = Vec3(0, 0, -1);
			vertex.tangent = Vec3(1, 0, 0);
			vertex.uv = Vec2(0, 0);
		}

		for (u32 i = 0; i < subdivision; ++i) {
			ctx.geometry->indices.push(i);
			ctx.geometry->indices.push((i + 1) % subdivision);
			ctx.geometry->indices.push(subdivision);
		}

		for (u32 i = 0; i < subdivision; ++i) {
			ctx.geometry->indices.push(subdivision + 1 + i);
			ctx.geometry->indices.push(subdivision + 1 + subdivision);
			ctx.geometry->indices.push(subdivision + 1 + (i + 1) % subdivision);
		}

		ctx.geometry->type = gpu::PrimitiveType::TRIANGLES;
		return Result::geom();
	}

	bool gui() override {
		ImGuiEx::NodeTitle("Cone");
		outputSlot(Result::GEOMETRY);
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
	SphereNode(EditorResource& resource) : Node(resource) {}

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
	
	Result eval(u16 output_idx, const Context& ctx) override {
		if (!ctx.geometry) return error("Invalid context");

		ctx.geometry->vertices.reserve(6 * subdivision * subdivision);
		ctx.geometry->indices.reserve(12 * (subdivision - 1));

		auto push_side = [&ctx, this](u32 coord0, u32 coord1, u32 coord2, float coord2_val){
			const u32 offset = ctx.geometry->vertices.size();
			for (u32 j = 0; j < subdivision; ++j) {
				for (u32 i = 0; i < subdivision; ++i) {
					Geometry::Vertex& v = ctx.geometry->vertices.emplace();
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
					ctx.geometry->indices.push(offset + i + j * subdivision);
					ctx.geometry->indices.push(offset + i + 1 + j * subdivision);
					ctx.geometry->indices.push(offset + i + (j + 1) * subdivision);

					ctx.geometry->indices.push(offset + i + (j + 1) * subdivision);
					ctx.geometry->indices.push(offset + i + 1 + j * subdivision);
					ctx.geometry->indices.push(offset + i + 1 + (j + 1) * subdivision);
				}
			}
		};

		push_side(0, 1, 2, 1);
		push_side(1, 0, 2, -1);

		push_side(2, 0, 1, 1);
		push_side(0, 2, 1, -1);
		
		push_side(1, 2, 0, 1);
		push_side(2, 1, 0, -1);
		ctx.geometry->type = gpu::PrimitiveType::TRIANGLES;

		return Result::geom();
	}

	bool gui() override {
		ImGuiEx::NodeTitle("Sphere");
		outputSlot(Result::GEOMETRY);
		bool res = ImGui::DragInt("Subdivision", (i32*)&subdivision);
		res = ImGui::DragFloat("Size", &size) || res;
		return res;
	}

	u32 subdivision = 10;
	float size = 1.f;
};

struct CubeNode : Node {
	CubeNode(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::CUBE; }
	bool hasInputPins() const override { return false; }
	bool hasOutputPins() const override { return true; }

	void serialize(OutputMemoryStream& blob) const override {
		blob.write(size);
	}

	void deserialize(InputMemoryStream& blob) override {
		blob.read(size);
	}

	Result eval(u16 output_idx, const Context& ctx) override {
		if (!ctx.geometry) return error("Invalid context");

		ctx.geometry->vertices.reserve(2 * size.x * size.y + 2 * size.y * size.z + 2 * size.x * size.z);
		const IVec3 s1 = size + IVec3(-1);
		ctx.geometry->indices.reserve(4 * s1.x * s1.y + 4 * s1.y * s1.z + 4 * s1.x * s1.z);

		auto push_side = [&ctx](u32 size_x, u32 size_y, u32 coord0, u32 coord1, u32 coord2, float coord2_val){
			const u32 offset = ctx.geometry->vertices.size();
			for (u32 j = 0; j < size_y; ++j) {
				for (u32 i = 0; i < size_x; ++i) {
					Geometry::Vertex& v = ctx.geometry->vertices.emplace();
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
					ctx.geometry->indices.push(offset + i + j * size_x);
					ctx.geometry->indices.push(offset + i + 1 + j * size_x);
					ctx.geometry->indices.push(offset + i + (j + 1) * size_x);

					ctx.geometry->indices.push(offset + i + (j + 1) * size_x);
					ctx.geometry->indices.push(offset + i + 1 + j * size_x);
					ctx.geometry->indices.push(offset + i + 1 + (j + 1) * size_x);
				}
			}
		};

		push_side(size.x, size.y, 0, 1, 2, 1);
		push_side(size.x, size.y, 1, 0, 2, -1);

		push_side(size.x, size.z, 2, 0, 1, 1);
		push_side(size.x, size.z, 0, 2, 1, -1);
		
		push_side(size.y, size.z, 1, 2, 0, 1);
		push_side(size.y, size.z, 2, 1, 0, -1);
		ctx.geometry->type = gpu::PrimitiveType::TRIANGLES;

		return Result::geom();
	}

	bool gui() override {
		ImGuiEx::NodeTitle("Cube");
		outputSlot(Result::GEOMETRY);
		bool res = ImGui::DragInt("Vertices X", &size.x);
		res = ImGui::DragInt("Vertices Y", &size.y) || res;
		res = ImGui::DragInt("Vertices Z", &size.z) || res;
		return res;
	}

	IVec3 size = IVec3(2);
};

struct MergeNode : Node {
	MergeNode(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::MERGE; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }

	static bool equal(double a, double b) { return fabsf(float(a - b)) < 0.001f; }
	static bool equal(float a, float b) { return fabsf(a - b) < 0.001f; }

	static bool equal(const Transform& a, const Transform& b) {
		return equal(a.pos.x, b.pos.x)
			&& equal(a.pos.y, b.pos.y)
			&& equal(a.pos.z, b.pos.z)
			&& equal(a.scale.x, b.scale.x)
			&& equal(a.scale.y, b.scale.y)
			&& equal(a.scale.z, b.scale.z)
			&& equal(a.rot.x, b.rot.x)
			&& equal(a.rot.y, b.rot.y)
			&& equal(a.rot.z, b.rot.z);
	}

	Result eval(u16 output_idx, const Context& ctx) override {
		if (!ctx.geometry) return error("Invalid context");

		if (!getInputGeometry(0, *ctx.geometry)) return error("Invalid input");

		Geometry tmp(m_allocator);
		if (!getInputGeometry(1, tmp)) return error("Invalid input");
		if (ctx.geometry->type != tmp.type) return error("Inputs do not have matching geometry types");

		if (!equal(ctx.geometry->to_world, tmp.to_world)) return error("Inputs have different world positions");

		const u32 old_size = ctx.geometry->vertices.size();

		ctx.geometry->vertices.resize(ctx.geometry->vertices.size() + tmp.vertices.size());
		memcpy(&ctx.geometry->vertices[old_size], tmp.vertices.begin(), tmp.vertices.byte_size());

		ctx.geometry->indices.reserve(ctx.geometry->indices.size() + tmp.indices.size());
		for (u32 idx : tmp.indices) {
			ctx.geometry->indices.push(idx + old_size);
		}

		return Result::geom();
	}

	bool gui() override {
		ImGuiEx::NodeTitle("Merge");
		ImGui::BeginGroup();
		inputSlot(Result::GEOMETRY); ImGui::TextUnformatted("A");
		inputSlot(Result::GEOMETRY); ImGui::TextUnformatted("B");
		ImGui::EndGroup();
		ImGui::SameLine();		
		outputSlot(Result::GEOMETRY);
		return false;
	}
};

struct TransformNode : Node {
	TransformNode(EditorResource& resource) : Node(resource) {}

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

	Result eval(u16 output_idx, const Context& ctx) override {
		if (!ctx.geometry) return error("Invalid context");

		if (!getInputGeometry(0, *ctx.geometry)) return error("Invalid input");

		Quat rotation;
		rotation.fromEuler(euler);

		// TODO transform normal and tangent
		for (Geometry::Vertex& v : ctx.geometry->vertices) {
			v.position = rotation.rotate(v.position) * scale + translation;
		}

		return Result::geom();
	}

	bool gui() override {
		ImGuiEx::NodeTitle("Transform");
		inputSlot(Result::GEOMETRY);
		ImGui::BeginGroup();
		bool res = ImGui::DragFloat3("Translation", &translation.x);
		res = ImGuiEx::InputRotation("Rotation", &euler.x) || res;
		res = ImGui::DragFloat3("Scale", &scale.x) || res;
		ImGui::EndGroup();
		ImGui::SameLine();
		outputSlot(Result::GEOMETRY);
		return res;
	}

	Vec3 translation = Vec3::ZERO;
	Vec3 euler = Vec3::ZERO;
	Vec3 scale = Vec3(1);
};

struct ScaleNode : Node {
	ScaleNode(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::SCALE; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }

	void serialize(OutputMemoryStream& blob) const override {
		blob.write(scale);
	}
	
	void deserialize(InputMemoryStream& blob) override {
		blob.read(scale);
	}

	Result eval(u16 output_idx, const Context& ctx) override {
		if (!ctx.geometry) return error("Invalid context");

		if (!getInputGeometry(0, *ctx.geometry)) return error("Invalid input");

		// TODO transform normal and tangent
		for (Geometry::Vertex& v : ctx.geometry->vertices) {
			v.position = v.position * scale;
		}

		return Result::geom();
	}

	bool gui() override {
		ImGuiEx::NodeTitle("Scale");
		inputSlot(Result::GEOMETRY);
		ImGui::BeginGroup();
		bool res = ImGui::DragFloat("X", &scale.x);
		res = ImGui::DragFloat("Y", &scale.y) || res;
		res = ImGui::DragFloat("Z", &scale.z) || res;
		ImGui::EndGroup();
		ImGui::SameLine();
		outputSlot(Result::GEOMETRY);
		return res;
	}

	Vec3 scale = Vec3(1);
};

struct GridNode : Node {
	GridNode(EditorResource& resource) : Node(resource) {}

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

	Result eval(u16 output_idx, const Context& ctx) override {
		if (!ctx.geometry) return error("Invalid context");

		ctx.geometry->type = gpu::PrimitiveType::TRIANGLES;
		ctx.geometry->vertices.resize((m_rows + 1) * (m_cols + 1));
		for (i32 j = 0; j < m_rows + 1; ++j) {
			for (i32 i = 0; i < m_cols + 1; ++i) {
				ctx.geometry->vertices[i + j * (m_cols + 1)] = {
					{ i / float(m_cols) * 2 - 1, j / float(m_rows) * 2 - 1, 0 },
					{ i / float(m_cols), j / float(m_rows) },
					{0, 0, 1},
					{1, 0, 0}
				};
			}
		}

		ctx.geometry->indices.reserve(m_rows * m_cols * 6);
		for (i32 j = 0; j < m_rows; ++j) {
			for (i32 i = 0; i < m_cols; ++i) {
				ctx.geometry->indices.push(i + j * (m_cols + 1));
				ctx.geometry->indices.push(i + 1 + j * (m_cols + 1));
				ctx.geometry->indices.push(i + (j + 1) * (m_cols + 1));

				ctx.geometry->indices.push(i + (j + 1) * (m_cols + 1));
				ctx.geometry->indices.push(i + 1 + j * (m_cols + 1));
				ctx.geometry->indices.push(i + 1 + (j + 1) * (m_cols + 1));
			}
		}

		return Result::geom();
	}

	bool gui() override {
		ImGuiEx::NodeTitle("Grid");
		outputSlot(Result::GEOMETRY);
		bool res = ImGui::DragInt("Columns", &m_cols);
		res = ImGui::DragInt("Rows", &m_rows) || res;
		return res;
	}

	i32 m_cols = 10;
	i32 m_rows = 10;
};

struct NoiseNode : Node {
	NoiseNode(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::NOISE; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }
	
	void serialize(OutputMemoryStream& blob) const override {
		blob.write(m_magic);
		blob.write(m_scale);
		blob.write(m_offset);
		blob.write(m_amplitude);
	}
	
	void deserialize(InputMemoryStream& blob) override {
		blob.read(m_magic);
		blob.read(m_scale);
		blob.read(m_offset);
		blob.read(m_amplitude);
	}
	
	static Vec2 hash(Vec2 p) {
		p = Vec2(dot(p, Vec2(127.1f, 311.7f)), dot(p, Vec2(269.5f, 183.3f)));
		return fract(sin(p) * 18.5453f);
	}

	static Vec2 floor(Vec2 p) { return { floorf(p.x), floorf(p.y) }; }
	static Vec2 sin(Vec2 p) { return { sinf(p.x), sinf(p.y) }; }
	static Vec2 fract(Vec2 p) { return p - floor(p); }
	static float mix(float a, float b, float t) { return a * (1 - t) + b * t; }
	
	// https://www.shadertoy.com/view/tldSRj
	float noise(Vec2 p) const {
		p = p * m_scale + Vec2(m_offset);
		const float kF = m_magic;

		Vec2 i = floor(p);
		Vec2 f = fract(p);
		f = f * f * (f * -2.f + 3.f);
		return m_amplitude * mix(mix(sinf(kF * dot(p, hash(i + Vec2(0.f, 0.f)))),
		               sinf(kF * dot(p, hash(i + Vec2(1.f, 0.f)))), f.x),
		               mix(sinf(kF * dot(p, hash(i + Vec2(0.f, 1.f)))),
		                   sinf(kF * dot(p, hash(i + Vec2(1.f, 1.f)))), f.x), f.y);
	}

	Result eval(u16 output_idx, const Context& ctx) override { 
		Result input = getInputResult(0, ctx);

		if (input.isValid()) {
			switch (input.type) {
				case Result::INVALID: ASSERT(false); break;
				case Result::GEOMETRY: return error("Invalid input");
				case Result::FLOAT:
					return Result(noise(Vec2(input.f_value)));
				case Result::VEC3:
					return Result(noise(input.vec3_value.xy()));
				case Result::I32:
					return Result(noise(Vec2((float)input.i32_value)));
			}
		}

		if (!ctx.vertex) return error("Invalid context");

		return Result(noise(ctx.vertex->position.xy()));
	}
	
	bool gui() override {
		ImGuiEx::NodeTitle("Noise");
		outputSlot();
		inputSlot(Result::VEC3);
		ImGui::TextUnformatted("Input vector");
		bool res = ImGui::DragFloat("Magic", &m_magic);
		res = ImGui::DragFloat("Scale", &m_scale) || res;
		res = ImGui::DragFloat("Offset", &m_offset) || res;
		res = ImGui::DragFloat("Amplitude", &m_amplitude) || res;
		return res;
	}

	float m_magic = 6.f;
	float m_scale = 1.f;
	float m_offset = 0.f;
	float m_amplitude = 1.f;
};

struct MakeVec3Node : Node {
	MakeVec3Node(EditorResource& resource) : Node(resource) {}
	
	NodeType getType() const override { return NodeType::MAKE_VEC3; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }
	void serialize(OutputMemoryStream& blob) const override {}
	void deserialize(InputMemoryStream& blob) override {}

	static float toFloat(const Result& val) {
		switch (val.type) {
			case Node::Result::INVALID: return 0;
			case Node::Result::FLOAT: return val.f_value;
			case Node::Result::I32: return (float)val.i32_value;
			default: ASSERT(false); return 0;
		}
	}

	Result eval(u16 output_idx, const Context& ctx) override {
		const Result iX = getInputResult(0, ctx);
		const Result iY = getInputResult(1, ctx);
		const Result iZ = getInputResult(2, ctx);
		
		float x = toFloat(iX);
		float y = toFloat(iY);
		float z = toFloat(iZ);

		return Result(Vec3(x, y, z));
	}
	
	bool gui() override {
		ImGuiEx::NodeTitle("Make vector3");
		outputSlot(Result::VEC3);
		inputSlot(Result::FLOAT);
		ImGui::TextUnformatted("X");
		inputSlot(Result::FLOAT);
		ImGui::TextUnformatted("Y");
		inputSlot(Result::FLOAT);
		ImGui::TextUnformatted("Z");
		return false;
	}
};

struct SnapToTerrainNode : Node {
	SnapToTerrainNode(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::SNAP_TO_TERRAIN; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }
	void serialize(OutputMemoryStream& blob) const override { blob.write(m_snap_normal); }
	void deserialize(InputMemoryStream& blob) override { blob.read(m_snap_normal); }
	
	Result eval(u16 output_idx, const Context& ctx) override {
		if (!ctx.geometry) return error("Invalid context");

		const Result val = getInputResult(0, ctx);
		if (!val.isValid() || val.type != Result::GEOMETRY) return error("Invalid input");

		World* world = m_resource.m_app.getWorldEditor().getWorld();
		ASSERT(world);
		RenderModule* render_module = (RenderModule*)world->getModule("renderer");
		
		for (EntityPtr e = render_module->getFirstTerrain(); e.isValid(); e = render_module->getNextTerrain(*e)) {
			const Terrain* terrain = render_module->getTerrain(*e);
			const Transform& terrain_to_geom = ctx.geometry->to_world.inverted() * world->getTransform(*e);
			const Transform& geom_to_terrain = terrain_to_geom.inverted();
			for (Geometry::Vertex& v : ctx.geometry->vertices) {
				v.position = Vec3(geom_to_terrain.transform(v.position));
				if (v.position.x >= 0 
					&& v.position.x <= terrain->m_width * terrain->m_scale.x
					&& v.position.y >= 0 
					&& v.position.y <= terrain->m_height * terrain->m_scale.z
				) {
					v.position.y = terrain->getHeight(v.position.x, v.position.z);
					if (m_snap_normal) {
						v.normal = terrain->getNormal(v.position.x, v.position.z);
						const Vec3 dir = cross(v.normal, v.tangent);
						v.tangent = normalize(cross(dir, v.normal));
					}
				}
				v.position = Vec3(terrain_to_geom.transform(v.position));
			}
		}

		return Result::geom();
	}
	
	bool gui() override {
		ImGuiEx::NodeTitle("Snap to terrain");
		inputSlot(Result::GEOMETRY);
		outputSlot(Result::GEOMETRY);
		ImGui::TextUnformatted("Geometry");
		return ImGui::Checkbox("Snap normal", &m_snap_normal);
	}

	bool m_snap_normal = false;
};

struct SplitVec3Node : Node {
	SplitVec3Node(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::SPLIT_VEC3; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }
	void serialize(OutputMemoryStream& blob) const override {}
	void deserialize(InputMemoryStream& blob) override {}
	
	Result eval(u16 output_idx, const Context& ctx) override {
		const Result val = getInputResult(0, ctx);
		if (!val.isValid()) return error("Invalid input");
		
		if (val.type != Result::VEC3) return error("Input is not vector3");

		switch (output_idx) {
			case 0: return Result(val.vec3_value.x);
			case 1: return Result(val.vec3_value.y);
			case 2: return Result(val.vec3_value.z);
			default: ASSERT(false); return Result(0.f);
		}
	}
	
	bool gui() override {
		ImGuiEx::NodeTitle("Split vector3");
		inputSlot(Result::VEC3);
		outputSlot(Result::FLOAT);
		ImGui::TextUnformatted("X");
		outputSlot(Result::FLOAT);
		ImGui::TextUnformatted("Y");
		outputSlot(Result::FLOAT);
		ImGui::TextUnformatted("Z");
		return false;
	}
};

struct RandomNode : Node {
	RandomNode(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::RANDOM; }
	bool hasInputPins() const override { return false; }
	bool hasOutputPins() const override { return true; }
	void serialize(OutputMemoryStream& blob) const override { blob.write(m_from); blob.write(m_to); }
	void deserialize(InputMemoryStream& blob) override { blob.read(m_from); blob.read(m_to); }
	
	Result eval(u16 output_idx, const Context& ctx) override {
		return Result(randFloat(m_from, m_to));
	}
	
	bool gui() override {
		ImGuiEx::NodeTitle("Random");
		outputSlot(Result::FLOAT);
		bool res = ImGui::DragFloat("From", &m_from);
		res = ImGui::DragFloat("To", &m_to) || res;
		return res;
	}

	float m_from = 0;
	float m_to = 1;
};

struct FloatConstNode : Node {
	FloatConstNode(EditorResource& resource) : Node(resource) {}
	
	NodeType getType() const override { return NodeType::FLOAT_CONST; }
	bool hasInputPins() const override { return false; }
	bool hasOutputPins() const override { return true; }
	void serialize(OutputMemoryStream& blob) const override { blob.write(m_value); }
	void deserialize(InputMemoryStream& blob) override { blob.read(m_value); }
	
	Result eval(u16 output_idx, const Context& ctx) override {
		return Result(m_value);
	}
	
	bool gui() override {
		ImGuiEx::NodeTitle("Float");
		outputSlot(Result::FLOAT);
		return ImGui::DragFloat("Value", &m_value);
	}

	float m_value = 0;
};

struct PinNode : Node {
	PinNode(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::PIN; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }
	void serialize(OutputMemoryStream& blob) const override {}
	void deserialize(InputMemoryStream& blob) override {}

	Result eval(u16 output_idx, const Context& ctx) override {
		return getInputResult(0, ctx);
	}
	
	bool gui() override {
		outputSlot();
		inputSlot();
		ImGui::TextUnformatted(" ");
		return false;
	}
};

struct MapRangeNode : Node {
	MapRangeNode(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::MAP_RANGE; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }
	void serialize(OutputMemoryStream& blob) const override { blob.write(m_from_range); blob.write(m_to_range); }
	void deserialize(InputMemoryStream& blob) override { blob.read(m_from_range); blob.read(m_to_range); }

	Result eval(u16 output_idx, const Context& ctx) override {
		const Result input = getInputResult(0, ctx);

		if (!input.isValid()) return error("Invalid input");
		if (input.type != Result::FLOAT) return error("Input is not float");

		const float rel = (input.f_value - m_from_range.x) / (m_from_range.y - m_from_range.x);
		return m_to_range.x + rel * (m_to_range.y - m_to_range.x);
	}
	
	bool gui() override {
		ImGuiEx::NodeTitle("Map range");
		outputSlot(Result::FLOAT);
		inputSlot(Result::FLOAT);
		bool res = ImGui::DragFloat("From min", &m_from_range.x);
		res = ImGui::DragFloat("From max", &m_from_range.y) || res;
		res = ImGui::DragFloat("To min", &m_to_range.x) || res;
		res = ImGui::DragFloat("To max", &m_to_range.y) || res;
		return res;
	}

	Vec2 m_from_range = Vec2(0, 1);
	Vec2 m_to_range = Vec2(-1, 1);
};

struct MixNode : Node {
	MixNode(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::MIX; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }
	void serialize(OutputMemoryStream& blob) const override { blob.write(m_ratio); }
	void deserialize(InputMemoryStream& blob) override { blob.read(m_ratio); }

	Result eval(u16 output_idx, const Context& ctx) override {
		const Result r0 = getInputResult(0, ctx);
		const Result r1 = getInputResult(1, ctx);
		const Result r2 = getInputResult(2, ctx);
		if (!r0.isValid() || !r1.isValid()) return error("Invalid input");

		if (r0.type != r1.type) return error("Inputs do not have matching types");
		if (r2.isValid() && r2.type != Result::FLOAT) return error("Ratio is not float");

		const float ratio = r2.type == Result::FLOAT ? r2.f_value : m_ratio;

		switch (r0.type) {
			case Result::INVALID: ASSERT(false); break;
			case Result::GEOMETRY: return error("Invalid input");
			case Result::I32:
				return i32(r0.i32_value + (r1.i32_value - r0.i32_value) * ratio);
			case Result::FLOAT:
				return r0.f_value + (r1.f_value - r0.f_value) * ratio;
			case Result::VEC3:
				return r0.vec3_value + (r1.vec3_value - r0.vec3_value) * ratio;
		}
		ASSERT(false);
		return {};
	}
	
	bool gui() override {
		ImGuiEx::NodeTitle("Mix");
		outputSlot();
		inputSlot();
		ImGui::TextUnformatted("A");
		inputSlot();
		ImGui::TextUnformatted("B");
		inputSlot();
		bool res = false;
		if (getInput(2)) {
			ImGui::TextUnformatted("Ratio");
		}
		else {
			res = ImGui::SliderFloat("Ratio", &m_ratio, 0, 1, "%.2f");
		}
		return res;
	}

	float m_ratio = 0.5f;
};

struct MathNode : Node {
	MathNode(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::MATH; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }
	void serialize(OutputMemoryStream& blob) const override { blob.write(m_operator); }
	void deserialize(InputMemoryStream& blob) override { blob.read(m_operator); }
	
	template <typename T0, typename T1> Result op(T0 v0, T1 v1) const { 
		switch (m_operator) {
			case SUBTRACT: return v0 - v1;
			case DIVIDE: return v0 / v1;
			case ADD: return v0 + v1;
			case MULTIPLY: return v0 * v1;
		}
		return {};
	}

	Result eval(u16 output_idx, const Context& ctx) override {
		const Result r0 = getInputResult(0, ctx);
		const Result r1 = getInputResult(1, ctx);
		if (!r0.isValid() || !r1.isValid()) return error("Invalid input");

		switch (r0.type) {
			case Result::GEOMETRY: return error("Invalid input");
			case Result::INVALID: ASSERT(false); break;
			case Result::I32:
				switch (r1.type) {
					case Result::GEOMETRY: return error("Invalid input");
					case Result::INVALID: ASSERT(false); break;
					case Result::I32: return op(r0.i32_value, r1.i32_value);
					case Result::FLOAT: return op(r0.i32_value, r1.f_value);
					case Result::VEC3: return op(r1.vec3_value, Vec3((float)r0.i32_value));
				}
				break;
			case Result::FLOAT:
				switch (r1.type) {
					case Result::GEOMETRY: return error("Invalid input");
					case Result::INVALID: ASSERT(false); break;
					case Result::I32: return op(r0.f_value, r1.i32_value);
					case Result::FLOAT: return op(r0.f_value, r1.f_value);
					case Result::VEC3: return op(r1.vec3_value, Vec3(r0.f_value));
				}
				break;
			case Result::VEC3:
				switch (r1.type) {
					case Result::GEOMETRY: return error("Invalid input");
					case Result::INVALID: ASSERT(false); break;
					case Result::I32: return op(r0.vec3_value, Vec3((float)r1.i32_value));
					case Result::FLOAT: return op(r0.vec3_value, Vec3(r1.f_value));
					case Result::VEC3: return op(r1.vec3_value, r0.vec3_value);
				}
				break;
		}
		ASSERT(false);
		return {};
	}
	
	bool gui() override {
		ImGuiEx::NodeTitle("Math");
		outputSlot();
		inputSlot();
		ImGui::TextUnformatted("A");
		inputSlot();
		ImGui::TextUnformatted("B");
		return ImGui::Combo("Operation", (i32*)&m_operator, "Add\0Multiply\0Divide\0Subtract\0");
	}

	enum Operator : u32 {
		ADD,
		MULTIPLY,
		DIVIDE,
		SUBTRACT
	};

	Operator m_operator = MULTIPLY;
};

struct SetNormalNode : Node {
	SetNormalNode(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::SET_NORMAL; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }
	void serialize(OutputMemoryStream& blob) const override {}
	void deserialize(InputMemoryStream& blob) override {}
	
	Result eval(u16 output_idx, const Context& in_ctx) override {
		if (!in_ctx.geometry) return error("Invalid context");
		if (!getInputGeometry(0, *in_ctx.geometry)) return error("Invalid input");
		
		const Input normal_input = getInput(1);
		if (!normal_input) return error("Invalid input");
		
		Node::Context ctx;
		for (Geometry::Vertex& v : in_ctx.geometry->vertices) {
			ctx.index = u32(&v - in_ctx.geometry->vertices.begin());
			ctx.vertex = &v;
			const Node::Result value_res = normal_input.eval(ctx);
			if (!value_res.isValid()) return error("Invalid input");
			if (value_res.type != Node::Result::VEC3) return error("Normal is not vector3");
			v.normal = value_res.vec3_value;
		}

		return Result::geom();
	}
	
	bool gui() override {
		ImGuiEx::NodeTitle("Set normal");
		outputSlot(Result::GEOMETRY);
		inputSlot(Result::GEOMETRY);
		ImGui::TextUnformatted("Geometry");
		inputSlot(Result::VEC3);
		ImGui::TextUnformatted("Normal");
		return false;
	}
};

struct SetPositionNode : Node {
	SetPositionNode(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::SET_POSITION; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }
	void serialize(OutputMemoryStream& blob) const override {}
	void deserialize(InputMemoryStream& blob) override {}
	
	Result eval(u16 output_idx, const Context& in_ctx) override {
		if (!in_ctx.geometry) return error("Invalid context");
		if (!getInputGeometry(0, *in_ctx.geometry)) return error("Invalid input");
		
		const Input abs_val = getInput(1);
		const Input rel_val = getInput(2);
		if (!abs_val && !rel_val) return error("Invalid inputs");
		
		Node::Context ctx;
		for (Geometry::Vertex& v : in_ctx.geometry->vertices) {
			ctx.index = u32(&v - in_ctx.geometry->vertices.begin());
			ctx.vertex = &v;
			if (abs_val) {
				const Node::Result value_res = abs_val.eval(ctx);
				if (!value_res.isValid()) return error("Invalid input");
				if (value_res.type != Node::Result::VEC3) return error("Position is not vector3");
				v.position = value_res.vec3_value;
			}
			if (rel_val) {
				const Node::Result value_res = rel_val.eval(ctx);
				if (!value_res.isValid()) return error("Invalid input");
				if (value_res.type != Node::Result::VEC3) return error("Offset is not vector3");
				v.position += value_res.vec3_value;
			}
		}

		return Result::geom();
	}
	
	bool gui() override {
		ImGuiEx::NodeTitle("Set position");
		outputSlot(Result::GEOMETRY);
		inputSlot(Result::GEOMETRY);
		ImGui::TextUnformatted("Geometry");
		inputSlot(Result::VEC3);
		ImGui::TextUnformatted("Position");
		inputSlot(Result::VEC3);
		ImGui::TextUnformatted("Offset");
		return false;
	}
};

struct RotatePointsNode : Node {
	RotatePointsNode(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::ROTATE_POINTS; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }
	void serialize(OutputMemoryStream& blob) const override {}
	void deserialize(InputMemoryStream& blob) override {}
	
	Result eval(u16 output_idx, const Context& in_ctx) override {
		if (!in_ctx.geometry) return error("Invalid context");
		if (!getInputGeometry(0, *in_ctx.geometry)) return error("Invalid input");
		
		const Input value = getInput(1);
		if (!value) return error("Invalid input");
		
		Node::Context ctx;
		for (Geometry::Vertex& v : in_ctx.geometry->vertices) {
			ctx.index = u32(&v - in_ctx.geometry->vertices.begin());
			ctx.vertex = &v;
			const Node::Result value_res = value.eval(ctx);
			if (!value_res.isValid()) continue;

			float angle = 0;
			switch (value_res.type) {
				case Node::Result::FLOAT: angle = value_res.f_value; break;
				case Node::Result::I32: angle = (float)value_res.i32_value; break;
				case Node::Result::GEOMETRY: return error("Invalid input");
				case Node::Result::VEC3: // TODO
				case Node::Result::INVALID: ASSERT(false); break;
			}
			float c = cosf(angle);
			float s = sinf(angle);
			Vec3 n = v.normal;
			Vec3 side = normalize(cross(v.tangent, n));
			v.normal = normalize(c * n + s * side);
		}

		return Result::geom();
	}
	
	bool gui() override {
		ImGuiEx::NodeTitle("Rotate points");
		outputSlot(Result::GEOMETRY);
		inputSlot(Result::GEOMETRY);
		ImGui::TextUnformatted("Geometry");
		inputSlot(Result::FLOAT);
		ImGui::TextUnformatted("Angle");
		
		return false;
	}
};

struct DistributePointsOnFacesNode : Node {
	DistributePointsOnFacesNode(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::DISTRIBUTE_POINT_ON_FACES; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return true; }
	
	void serialize(OutputMemoryStream& blob) const override {
		blob.write(density);
	}
	
	void deserialize(InputMemoryStream& blob) override {
		blob.read(density);
	}

	Result eval(u16 output_idx, const Context& ctx) override {
		if (!ctx.geometry) return error("Invalid context");

		Geometry geom(m_allocator);

		if (!getInputGeometry(0, geom)) return error("Invalid input");
		if (geom.type != gpu::PrimitiveType::TRIANGLES) return error("Input is not a triangle mesh");

		float total_area = 0;
		for (u32 i = 0; i < (u32)geom.indices.size(); i += 3) {
			const u32 idx0 = geom.indices[i];
			const u32 idx1 = geom.indices[i + 1];
			const u32 idx2 = geom.indices[i + 2];
			const Vec3 v0 = geom.vertices[idx1].position - geom.vertices[idx0].position;
			const Vec3 v1 = geom.vertices[idx2].position - geom.vertices[idx0].position;
			total_area += length(cross(v0, v1));
		}

		ctx.geometry->vertices.reserve(u32(density * total_area));
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
					Geometry::Vertex& v = ctx.geometry->vertices.emplace();
					v.position = geom.vertices[idx0].position + uv.x * v0 + uv.y * v1;
					v.normal = normalize(cross(v0, v1));
					v.tangent = v0;
					break;
				}
			}
		}

		ctx.geometry->type = gpu::PrimitiveType::POINTS;
		ctx.geometry->to_world = geom.to_world;
		return Result::geom();
	}

	bool gui() override {
		ImGuiEx::NodeTitle("Distribute points on faces");
		inputSlot(Result::GEOMETRY);
		bool res = ImGui::DragFloat("Density", &density);
		ImGui::SameLine();
		outputSlot(Result::GEOMETRY);
		return res;
	}

	float density = 1.f;
};

struct OutputNode : Node {
	OutputNode(EditorResource& resource) : Node(resource) {}

	NodeType getType() const override { return NodeType::OUTPUT; }
	bool hasInputPins() const override { return true; }
	bool hasOutputPins() const override { return false; }
	
	Result eval(u16 output_idx, const Context& ctx) override {
		if (!ctx.geometry) return error("Invalid context");

		if (!getInputGeometry(0, *ctx.geometry)) return error("Invalid input");

		return Result::geom();
	}

	void serialize(OutputMemoryStream& blob) const override {
		blob.write(user_channels_count);
	}

	void deserialize(InputMemoryStream& blob) override {
		blob.read(user_channels_count);
	}

	bool gui() override {
		ImGuiEx::NodeTitle("Output geometry");
		inputSlot(Result::GEOMETRY);
		return ImGui::SliderInt("User channels", (i32*)&user_channels_count, 0, 4);
	}

	u32 user_channels_count = 0;
};

} // anonymous namespace

static const ComponentType MODEL_INSTANCE_TYPE = reflection::getComponentType("model_instance");

struct ProceduralGeomGeneratorPlugin : StudioApp::GUIPlugin, NodeEditor {
	ProceduralGeomGeneratorPlugin(StudioApp& app)
		: m_app(app)
		, m_allocator(app.getAllocator())
		, NodeEditor(app.getAllocator())
	{
		m_apply_action.init("Apply", "Procedural geometry editor apply", "proc_geom_editor_apply", ICON_FA_CHECK, os::Keycode::E, Action::Modifiers::CTRL, Action::IMGUI_PRIORITY);

		m_toggle_ui.init("Procedural editor", "Toggle procedural editor", "procedural_editor", "", Action::IMGUI_PRIORITY);
		m_toggle_ui.func.bind<&ProceduralGeomGeneratorPlugin::toggleOpen>(this);
		m_toggle_ui.is_selected.bind<&ProceduralGeomGeneratorPlugin::isOpen>(this);
		
		app.addWindowAction(&m_toggle_ui);
		app.addAction(&m_apply_action);
		app.getSettings().registerPtr("is_procedural_geom_editor_open", &m_is_open);
		newGraph();
	}

	~ProceduralGeomGeneratorPlugin(){
		m_app.removeAction(&m_toggle_ui);
		m_app.removeAction(&m_apply_action);
		if (m_resource) LUMIX_DELETE(m_allocator, m_resource);
	}

	bool onAction(const Action& action) override {
		const CommonActions& actions = m_app.getCommonActions();
		if (&actions.del == &action) deleteSelectedNodes();
		else if (&action == &m_apply_action) apply();
		else if (&action == &actions.save) save();
		else if (&action == &actions.undo) undo();
		else if (&action == &actions.redo) redo();
		else return false;
		return true;
	}

	void colorLinks() {
		const ImU32 colors[] = {
			IM_COL32(0x20, 0x20, 0xA0, 0xFF),
			IM_COL32(0x20, 0xA0, 0x20, 0xFF),
			IM_COL32(0x20, 0xA0, 0xA0, 0xFF),
			IM_COL32(0xA0, 0x20, 0x20, 0xFF),
			IM_COL32(0xA0, 0x20, 0xA0, 0xFF),
			IM_COL32(0xA0, 0xA0, 0x20, 0xFF),
			IM_COL32(0xA0, 0xA0, 0xA0, 0xFF),
		};
	
		for (i32 i = 0, c = m_resource->m_links.size(); i < c; ++i) {
			Link& l = m_resource->m_links[i];
			l.color = colors[i % lengthOf(colors)];
		}
	}

	void pushUndo(u32 tag) override {
		if (m_autoapply) apply();
		colorLinks();
		m_resource->markReachableNodes();
		SimpleUndoRedo::pushUndo(tag);
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

	void onLinkDoubleClicked(Link& link, ImVec2 pos) override {
		Node* n = addNode(NodeType::PIN, pos, false);
		Link new_link;
		new_link.from = n->m_id | OUTPUT_FLAG; 
		new_link.to = link.to;
		link.to = n->m_id;
		m_resource->m_links.push(new_link);
		pushUndo(NO_MERGE_UNDO);
	}

	void onContextMenu(ImVec2 pos) override {
		Node* new_node = nullptr;
		static char filter[64] = "";
		if (ImGui::IsWindowAppearing()) ImGui::SetKeyboardFocusHere();
		ImGui::SetNextItemWidth(200);
		ImGui::InputTextWithHint("##filter", "Filter", filter, sizeof(filter), ImGuiInputTextFlags_AutoSelectAll);
		for (const auto& t : TYPES) {
			StaticString<64> label(t.label);
			if (t.key) {
				label.append(" (LMB + ", t.key, ")");
			}

			if ((!filter[0] || findInsensitive(t.label, filter)) && (ImGui::IsKeyPressed(ImGuiKey_Enter) || ImGui::MenuItem(label))) {
				new_node = addNode(t.type, pos, true);
				filter[0] = '\0';
				ImGui::CloseCurrentPopup();
				break;
			}
		}
		if (new_node) {
			new_node->m_pos = pos;
			pushUndo(NO_MERGE_UNDO);
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
	bool hasFocus() const override { return m_has_focus; }

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
		pushUndo(NO_MERGE_UNDO);
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
	}

	void newGraph() {
		clear();
		m_path = "";
		addNode(NodeType::OUTPUT, ImVec2(100, 100), false);
		pushUndo(NO_MERGE_UNDO);
	}

	void deleteUnreachable() {
		m_resource->deleteUnreachable();
		pushUndo(NO_MERGE_UNDO);
	}

	void onGUI() override {
		m_has_focus = false;
		if (!m_is_open) return;

		ImGui::SetNextWindowSize(ImVec2(200, 200), ImGuiCond_FirstUseEver);
		if (ImGui::Begin("Procedural geometry", &m_is_open, ImGuiWindowFlags_MenuBar)) {
			m_has_focus = ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows);
			if (ImGui::BeginMenuBar()) {
				const CommonActions& actions = m_app.getCommonActions();
				if (ImGui::BeginMenu("File")) {
					if (ImGui::MenuItem("New")) newGraph();
					if (ImGui::MenuItem("Open")) m_show_open = true;
					if (menuItem(actions.save, true)) save();
					if (ImGui::MenuItem("Save As")) m_show_save_as = true;
					if (menuItem(m_apply_action, canApply())) apply();
					ImGui::MenuItem("Autoapply", nullptr, &m_autoapply);
					ImGui::EndMenu();
				}
				if (ImGui::BeginMenu("Edit")) {
					if (menuItem(actions.undo, canUndo())) undo();
					if (menuItem(actions.redo, canRedo())) redo();
					if (ImGui::MenuItem(ICON_FA_BRUSH "Clear")) deleteUnreachable();
					ImGui::EndMenu();
				}
				ImGui::EndMenuBar();
			}

			FileSelector& fs = m_app.getFileSelector();
			if (fs.gui("Open", &m_show_open, "pgm", false)) open(fs.getPath());
			if (fs.gui("Save As", &m_show_save_as, "pgm", true)) saveAs(fs.getPath());

			m_app.getAssetBrowser().resourceInput("material", m_resource->m_material, Material::TYPE);
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
	bool m_autoapply = false;
	bool m_has_focus = false;
	Action m_toggle_ui;
	Action m_apply_action;
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

Node::Result Node::error(const char* msg) {
	m_error = msg;
	return {};
}


bool Node::getInputGeometry(u16 input_idx, Geometry& geom) const {
	Context ctx;
	ctx.geometry = &geom;
	const Result res = getInputResult(input_idx, ctx);
	return res.type == Result::GEOMETRY;
}

Node::Result Node::getInputResult(u16 input_idx, const Context& ctx) const {
	Input input = getInput(input_idx);
	return input.eval(ctx);
}

Input Node::getInput(u16 input_idx) const {
	Input res;
	forEachInput(m_resource, m_id, [&](Node* from, u16 from_attr, u16 to_attr, u32 link_idx){
		if (to_attr == input_idx) {
			res.output_idx = from_attr;
			res.node = static_cast<Node*>(from);
		}
	});
	return res;
}

Node::Node(EditorResource& resource)
	: m_resource(resource)
	, m_error(resource.m_allocator)
	, m_allocator(resource.m_allocator)
{}

void ProceduralGeomGeneratorPlugin::apply() {
	const Array<EntityRef>& selected = m_app.getWorldEditor().getSelectedEntities();
	if (selected.size() != 1) return;
	
	World* world = m_app.getWorldEditor().getWorld();
	bool children_removed = false;
	for (Node* node : m_resource->m_nodes) {
		node->m_error = "";
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

	OutputNode* output = (OutputNode*)m_resource->m_nodes[0];
	Geometry geom(m_allocator);
	Node::Context ctx;
	ctx.geometry = &geom;
	const Node::Result res = output->eval(0, ctx);
	if (res.type != Node::Result::GEOMETRY) return;

	RenderModule* module = (RenderModule*)world->getModule("renderer");
	
	if (!world->hasComponent(selected[0], PROCEDURAL_GEOM_TYPE)) world->createComponent(PROCEDURAL_GEOM_TYPE, selected[0]);

	Span<const u8> vertices((const u8*)geom.vertices.begin(), (u32)geom.vertices.byte_size());
	Span<const u8> indices((const u8*)geom.indices.begin(), (u32)geom.indices.byte_size());
	gpu::VertexDecl decl(geom.type);
	decl.addAttribute(0, 3, gpu::AttributeType::FLOAT, 0);  // pos
	decl.addAttribute(12, 2, gpu::AttributeType::FLOAT, 0); // uv
	decl.addAttribute(20, 3, gpu::AttributeType::FLOAT, 0); // normal
	decl.addAttribute(32, 3, gpu::AttributeType::FLOAT, 0); // tangent
	OutputMemoryStream user_vertices(m_allocator);
	if (output->user_channels_count > 0) {
		decl.addAttribute(44, output->user_channels_count, gpu::AttributeType::U8, gpu::Attribute::NORMALIZED);
		user_vertices.reserve(geom.vertices.size() * (sizeof(Geometry::Vertex) + output->user_channels_count * sizeof(float))); 
		for (u32 i = 0; i < (u32)geom.vertices.size(); ++i) {
			user_vertices.write(geom.vertices[i]);
			const u8 v[] = { 0xff, 0, 0xff, 0xff };
			user_vertices.write(&v, sizeof(u8) * output->user_channels_count);
		}
		vertices = user_vertices;
	}
	module->setProceduralGeometry(selected[0], vertices, decl, indices, gpu::DataType::U32);
	module->setProceduralGeometryMaterial(selected[0], Path(m_resource->m_material));

}

void EditorResource::markReachable(Node& n) {
	n.m_reachable = true;
	forEachInput(*this, n.m_id, [&](Node* from, u16 from_attr, u16 to_attr, u32 link_idx){
		markReachable(*from);
	});
} 


Node* EditorResource::createNode(NodeType type, ImVec2 pos) {
	Node* node;
	switch (type) {
		case NodeType::CIRCLE: node = LUMIX_NEW(m_allocator, CircleNode)(*this); break;
		case NodeType::CONE: node = LUMIX_NEW(m_allocator, ConeNode)(*this); break;
		case NodeType::CUBE: node = LUMIX_NEW(m_allocator, CubeNode)(*this); break;
		case NodeType::CURVE: node = LUMIX_NEW(m_allocator, CurveNode)(*this); break;
		case NodeType::CYLINDER: node = LUMIX_NEW(m_allocator, CylinderNode)(*this); break;
		case NodeType::DISTRIBUTE_POINT_ON_FACES: node = LUMIX_NEW(m_allocator, DistributePointsOnFacesNode)(*this); break;
		case NodeType::EXTRUDE_ALONG: node = LUMIX_NEW(m_allocator, ExtrudeAlongNode)(*this); break;
		case NodeType::FLOAT_CONST: node = LUMIX_NEW(m_allocator, FloatConstNode)(*this); break;
		case NodeType::GRID: node = LUMIX_NEW(m_allocator, GridNode)(*this); break;
		case NodeType::INDEX: node = LUMIX_NEW(m_allocator, IndexNode)(*this); break;
		case NodeType::INSTANTIATE_PREFAB: node = LUMIX_NEW(m_allocator, InstantiatePrefabNode)(*this); break;
		case NodeType::LINE: node = LUMIX_NEW(m_allocator, LineNode)(*this); break;
		case NodeType::MAKE_VEC3: node = LUMIX_NEW(m_allocator, MakeVec3Node)(*this); break;
		case NodeType::PIN: node = LUMIX_NEW(m_allocator, PinNode)(*this); break;
		case NodeType::MAP_RANGE: node = LUMIX_NEW(m_allocator, MapRangeNode)(*this); break;
		case NodeType::MIX: node = LUMIX_NEW(m_allocator, MixNode)(*this); break;
		case NodeType::NOISE: node = LUMIX_NEW(m_allocator, NoiseNode)(*this); break;
		case NodeType::MERGE: node = LUMIX_NEW(m_allocator, MergeNode)(*this); break;
		case NodeType::MODEL: node = LUMIX_NEW(m_allocator, ModelNode)(*this); break;
		case NodeType::MATH: node = LUMIX_NEW(m_allocator, MathNode)(*this); break;
		case NodeType::RANDOM: node = LUMIX_NEW(m_allocator, RandomNode)(*this); break;
		case NodeType::OUTPUT: node = LUMIX_NEW(m_allocator, OutputNode)(*this); break;
		case NodeType::POINT: node = LUMIX_NEW(m_allocator, PointNode)(*this); break;
		case NodeType::POSITION: node = LUMIX_NEW(m_allocator, PositionNode)(*this); break;
		case NodeType::PLACE_INSTANCES_AT_POINTS: node = LUMIX_NEW(m_allocator, PlaceInstancesAtPoints)(*this); break;
		case NodeType::ROTATE_POINTS: node = LUMIX_NEW(m_allocator, RotatePointsNode)(*this); break;
		case NodeType::SCALE: node = LUMIX_NEW(m_allocator, ScaleNode)(*this); break;
		case NodeType::SET_NORMAL: node = LUMIX_NEW(m_allocator, SetNormalNode)(*this); break;
		case NodeType::SET_POSITION: node = LUMIX_NEW(m_allocator, SetPositionNode)(*this); break;
		case NodeType::SPHERE: node = LUMIX_NEW(m_allocator, SphereNode)(*this); break;
		case NodeType::SPLINE: node = LUMIX_NEW(m_allocator, SplineNode)(*this); break;
		case NodeType::NORMAL: node = LUMIX_NEW(m_allocator, NormalNode)(*this); break;
		case NodeType::SPLIT_VEC3: node = LUMIX_NEW(m_allocator, SplitVec3Node)(*this); break;
		case NodeType::SNAP_TO_TERRAIN: node = LUMIX_NEW(m_allocator, SnapToTerrainNode)(*this); break;
		case NodeType::SPIRAL: node = LUMIX_NEW(m_allocator, SpiralNode)(*this); break;
		case NodeType::TRANSFORM: node = LUMIX_NEW(m_allocator, TransformNode)(*this); break;
		default: ASSERT(false); return nullptr;
	}
	node->m_pos = pos;
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
	PROFILE_FUNCTION();
	auto* plugin = LUMIX_NEW(app.getAllocator(), ProceduralGeomGeneratorPlugin)(app);
	app.addPlugin(*plugin);
	return nullptr;
}

} // namespace Lumix

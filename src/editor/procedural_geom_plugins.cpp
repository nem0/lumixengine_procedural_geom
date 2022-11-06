#define LUMIX_NO_CUSTOM_CRT
#include "editor/studio_app.h"
#include "editor/world_editor.h"
#include "engine/allocator.h"

#include "imgui/imgui.h"


using namespace Lumix;

struct ProceduralGeomPlugin : StudioApp::GUIPlugin {
	ProceduralGeomPlugin(StudioApp& app) : m_app(app) {}

	void onWindowGUI() override {
		ImGui::SetNextWindowSize(ImVec2(200, 200), ImGuiCond_FirstUseEver);
		if (ImGui::Begin("Procedural geometry")) {
			ImGui::TextUnformatted("Hello world");
		}
		ImGui::End();
	}
	
	const char* getName() const override { return "procedural_geom"; }

	StudioApp& m_app;
};


LUMIX_STUDIO_ENTRY(procedural_geom) {
	auto* plugin = LUMIX_NEW(app.getAllocator(), ProceduralGeomPlugin)(app);
	app.addPlugin(*plugin);
	return nullptr;
}

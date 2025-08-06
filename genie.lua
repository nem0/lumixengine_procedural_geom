if plugin "procedural_geom" then
	files { 
		"src/**.c",
		"src/**.cpp",
		"src/**.h",
		"genie.lua"
	}
	defines { "BUILDING_PROCEDURAL_GEOM" }
	dynamic_link_plugin { "engine", "core", "renderer" }
	if build_studio then
		dynamic_link_plugin { "editor" }
	end
end
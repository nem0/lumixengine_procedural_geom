project "procedural_geom"
	libType()
	files { 
		"src/**.c",
		"src/**.cpp",
		"src/**.h",
		"genie.lua"
	}
	defines { "BUILDING_PROCEDURAL_GEOM" }
	links { "engine", "core", "renderer" }
	if build_studio then
		links { "editor" }
	end
	useLua()
	defaultConfigurations()

linkPlugin("procedural_geom")
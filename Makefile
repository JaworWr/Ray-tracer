all: ray-tracer

ray-tracer: Main.hs DataTypes.hs Geometry.hs Scene.hs SceneParser.hs
	cabal build
clean:
	cabal clean



all: ray-tracer

ray-tracer: Main.hs DataTypes.hs Geometry.hs Scene.hs SceneParser.hs
	ghc -o ray-tracer Main.hs

clean:
	rm -f *.o *.hi ray-tracer



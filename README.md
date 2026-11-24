# mk52

A CAD kernel and a local page to author with it. A part is OCaml code: functions that build a solid from features. The kernel compiles a solid to one evaluator, and everything is read from that: the picture, the edges, the mesh. The page opens in your browser, beside your editor, and draws the model on the graphics card with WebGPU.

mk52 was copied from `pointer_mk49` on 24 November 2026: its kernel, its command and its tests, without the code that rebuilt parts from STEP files. `pointer_mk49/README.md` has the history: what each part of the kernel showed in its experiments, the extraction audit, the mesher, and how the shader's gates were found.

## Use

```sh
./mk52 install                # once: puts mk52 on the path
mkdir designs && cd designs
mk52 init                     # this folder is a design folder now
mk52 new lamp base            # a project, lamp/, and its first file, lamp/base.ml: a block to start from
mk52 open                     # the page, in the browser
mk52 view lamp.base           # build a function and show it in the page, in front
```

Edit `lamp/base.ml` in your editor; every time a file is saved the project is built again and the page redraws what is open. What does not compile is said in the page, under the lists.

| Command | What it does |
|---|---|
| `mk52 init` | makes the folder a design folder: a dune project, a link `mk52` to the tool, and `.mcp.json` for an agent |
| `mk52 new <project> <file>` | a new file with one function, a block to start from; the project's folder and build file if they are not there |
| `mk52 open` | serves the page here if it is not served yet, and opens it in the browser (`$BROWSER`, or the system's own) |
| `mk52 view <model>` | builds a model and shows it in the page, in front; opens the page if none is open |
| `mk52 models` | the projects here and their functions; `mk52 models shapes` lists the tool's own small shapes |
| `mk52 close <model> ...` | closes those models in the page |
| `mk52 status`, `mk52 stop` | whether the page is served, where, and what is open; stops serving it |
| `mk52 picture <model> [...]` | a picture made by the kernel, with no page and no graphics card: a PNG whose path is printed. `yaw=` `pitch=` `distance=` `target=x,y,z` `edges` `mesh` `size=800x600` `out=` |
| `mk52 extract <model> [32\|64\|128]` | its edges and corners, printed with every flag, and drawn in the page if the model is open |
| `mk52 mesh <model> [cells] [raw]` | its surface as even triangles attached to its creases and corners: an STL file, what the mesh is like, and a picture |
| `mk52 brief <model>` | what an agent working on that function is told |
| `mk52 mcp` | the kernel and the page as tools for an agent |

A model is a function that takes `()` and returns a solid: `<project>.<file>.<function>`, or `<project>.<file>` for a file's `graph` or its only function. A model is looked at in the box of its own surface, which the octree finds; it must lie within 10 m of the origin.

## The page

- **The picture** is a ray a pixel, marched through a shader the kernel writes for the model in WGSL (`lib/shader.ml`). Each face has its own colour.
- **The shader skips what cannot matter at a point.** The model's chains (a union of a list, a list of cuts) are written in gates of 240 operations or more, each entered only where the octree says it can matter: a bit a gate for each cell of a grid over the model. Inside a gate everything is computed in full, so the field is the model's field; `dune test` holds that. On an M1 Max a skipped block costs 2% of running it at that size and 45% at 30 operations, which is why gates are large.
- **Beside the picture:** the models that are open, each project's functions (click one to open it), and the features of the model in front, nested as the code made them. A feature clicked is picked out and the rest greys; its faces are listed under it and can be picked out in turn. Alone shows a feature by itself; the file and line open it in VS Code.
- **A click on the model** asks the kernel what is there: the face, the point, and the feature that made it, with its file and line.
- **Over the model:** the outlines of its sketches, the frames its features sit in, and its edges and corners once they are extracted (Find edges, or `mk52 extract`): sharp edges dark, smooth ones grey, corners as dots, flags as red dots, all faint where the model hides them.
- **The camera:** dragging turns the view about a point, shift-drag or right-drag slides it, the wheel moves the eye towards what is under the pointer. F fits the model; Escape clears what is picked out.
- **Under the lists:** how large the model is and how long the graphics card took over the last picture, where the browser will say.

It needs a browser with WebGPU: a recent Chrome, Edge or Safari.

## How the parts meet

Each part has one job. The command (`cli/`, OCaml) builds, measures and draws pictures, and exits. The page (`web/view.html`) is the window, and holds no model. Between them a small server (`mk52 serve`, started by `mk52 open`) stays for as long as the page is wanted: it listens on this machine only, and does no more than hand the page what is in the design folder's `_mk52/` and run the command for what the page needs worked out. So a kernel built again is the kernel the next click gets, and the page is read from the tool each time it is loaded: neither needs the server started again.

| In `_mk52/` | Written by | What it is |
|---|---|---|
| `open/<project>.functions` | the command | a project's functions that can be built alone |
| `open/<model>.model` | the command | a model as last built; it is open in the page for as long as this file is there |
| `open/<model>.edges.json` | the command | its edges and corners, if extracted |
| `open/<model>.<feature>.gates` | the command | where its shader's gates are entered |
| `front` | the command | the model the page is to bring to the front |
| `state/<model>.json` | the server, from the page | what is picked out in it and where the camera is, for an agent |
| `server` | the server | its process and its port (8752, or the next one free) |
| `seen` | the server | written while a page is looking, so `mk52 view` opens one only when none is |

The server looks at the design's code a few times a second, and runs `mk52 refresh` when a file was saved. The page asks it what is in the folder as often. A model written: the page asks for `mk52 scene <model>` (how it is drawn: box, shader, features, sketches) and compiles the shader. A click: `mk52 pick`.

## What is in it

| File | Lines | What it holds |
|---|---|---|
| `lib/expr.ml`, `graph.ml`, `solid.ml`, `frame.ml` | 367 | formulas; the graph of pieces, applies and selects, with features; primitives as true distances; frames |
| `lib/sketch.ml` | 368 | a closed outline of lines and arcs as its exact distance, pulled, turned or lofted into a solid |
| `lib/spline.ml`, `fit.ml` | 312 | free-form faces: a B-spline height patch, and fitted cubic patches |
| `lib/prog.ml` | 290 | the graph compiled to arrays, and the one evaluator |
| `lib/octree.ml` | 132 | top-down pruning by slope bound: every cell with its decisions |
| `lib/extract.ml` | 674 | edges and corners: seed, walk, cut, assemble, flag |
| `lib/surface.ml` | 697 | a triangle mesh of the surface, attached to its creases and corners |
| `lib/shader.ml` | 240 | the compiled program as a WGSL function, its chains in gates, and the table of where each gate is entered |
| `lib/render.ml`, `png.ml` | 207 | a picture made by the kernel, as a PNG |
| `lib/emit.ml`, `place.ml`, `json.ml` | 215 | a model as a file; where things are kept; JSON |
| `cli/main.ml` | 226 | the `mk52` command |
| `cli/project.ml` | 316 | models by name, built: a project compiled once and any of its functions built from it |
| `cli/scene.ml` | 117 | what the page asks: how a model is drawn, what a ray hits |
| `cli/serve.ml` | 204 | the page's server: the folder's files, the command run, the code watched |
| `cli/mcp.ml` | 145 | tools for an agent: `picture`, `kernel_probe`, `kernel_extract`, `viewer_state`, `feature_brief` |
| `web/view.html` | 647 | the page: its shaders (the march, lines and dots), the drawing, the camera, the lists, the mouse |
| `models/`, `test/`, `experiments/`, `bench/` | | the kernel's test shapes, its 191 checks (`dune test`, 15 s), and mk49's experiments |

## Not there

- **Nothing of STEP files:** no reference, no deviation, no check against faces, no fitting. That stays in `pointer_mk49`.
- **The mesh is not drawn in the page.** `mk52 mesh` writes the STL file and a picture.
- **A native app.** The first mk52 had one, in Swift with Metal. WebGPU drew the same shader as fast (a synth's assembly of 4,912 nodes at 2200 x 1800 on an M1 Max: 14.7, 41.3 and 85.9 ms with the whole model in view, filling the view and close up, against 14.8, 42.3 and 86.1 ms natively), and a page can be driven and checked by an agent, which a native window cannot.
- **Frame times are measured on that one model only,** in a test page that drew pictures one straight after another. The page's own figure for a single picture after a pause has read higher (23 ms for the first of those three views); why is not yet known.
- **A large shader takes a second or more to compile** (1.3 s for that assembly). The page keeps showing the model as it was until the new shader is ready.
- **Tried in one browser only:** Chromium, on a Mac. Nothing tests the page by itself; it was driven by hand through its lists, the mouse, picking, Find edges and a rebuild on save.

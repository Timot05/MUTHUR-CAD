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
| `mk52 outline <model> [out=file.dxf]` | the outline of a flat part, a board's: the edges round its top, holes too, as the lines, arcs and circles they are, in a DXF file in mm |
| `mk52 mesh <model> [cells] [raw]` | its surface as even triangles attached to its creases and corners: an STL file, what the mesh is like, and a picture |
| `mk52 brief <model>` | what an agent working on that function is told |
| `mk52 mcp` | the kernel and the page as tools for an agent |

A model is a function that takes `()` and returns a solid: `<project>.<file>.<function>`, or `<project>.<file>` for a file's `graph` or its only function. A model is looked at in the box of its own surface, which the octree finds; it must lie within 10 m of the origin.

## The page

- **The picture** is a ray a pixel, marched through a shader the kernel writes for the model in WGSL (`lib/shader.ml`). Each face has its own colour.
- **The shader skips what cannot matter at a point.** The model's chains (a union of a list, a list of cuts) are written in gates of 240 operations or more, each entered only where the octree says it can matter: a bit a gate for each cell of a grid over the model. Inside a gate everything is computed in full, so the field is the model's field; `dune test` holds that. On an M1 Max a skipped block costs 2% of running it at that size and 45% at 30 operations, which is why gates are large.
- **Beside the picture:** the models that are open, each project's functions under their files (click one to open it), and the features of the model in front, nested as the code made them. A feature clicked is picked out and the rest greys; its faces are listed under it and can be picked out in turn. Each feature has two switches at its right end. The eye takes it out of the model and puts it back: the kernel draws the graph with that feature as nothing, so a part leaves an assembly, a cut no longer cuts, and a body leaves with its cuts. The square shows it alone, even inside a feature that is taken out. Whole model's two switches put everything back. The file and line open a feature in VS Code.
- **The look** is square and ruled: sections, groups of buttons and the boxes over the picture are separated by one black line, nothing is rounded or shadowed. Its colours, type and spacing are tokens at the top of `web/view.html`, in light and dark.
- **A click on the model** asks the kernel what is there: the face, the point, and the feature that made it, with its file and line.
- **Over the model:** the outlines of its sketches, the frames its features sit in, and its edges and corners once they are extracted (Find edges, or `mk52 extract`): sharp edges dark, smooth ones grey, corners as dots, flags as red dots, all faint where the model hides them.
- **The camera:** dragging turns the view about a point, shift-drag or right-drag slides it, the wheel moves the eye towards what is under the pointer. F fits the model; Escape clears what is picked out.
- **Under the lists:** how large the model is and how long the graphics card took over the last picture, where the browser will say.
- **Regions** draws the model another way (off until it is switched on, or the page's address ends in `?regions`): cut into some 500 boxes, each drawn by a short shader of its own that holds only what matters inside it; the one compiled shader then gives each pixel its face. It is five times faster on one of the two large models it was tried on and twice as fast on the other, and takes seconds longer to be ready, during which the one compiled shader draws alone. See "Regions, built" below.
- **Assets:** what a model shows from outside, a STEP file or an STL mesh, drawn with the model where its code puts it, each hiding the other by depth. A model says `Asset.show ~frame "../parts/switch.step"`, in a frame like a feature, the path from the design folder. A STEP file is cut into triangles the first time it is shown (1.3 s for a key switch of 1.3 MB, 76,000 triangles) and kept in `_mk52/assets/` until the file changes. An asset is seen only: it is no part of the model's field, so nothing cuts it or is cut by it, a click goes through it, and `mk52 picture` does not draw it.
- **Colour:** a feature is given one where it is made, `Graph.feature ~colour:"#2e6b4d" __POS__ "board" ...`, and what is made inside it has it too; an asset likewise, `Asset.show ~colour`. With Faces off the page shows every part in its colour, and grey where its code gives none.
- **While the view is moved** the picture is drawn with fewer pixels, and in full again a moment after: as many fewer as it takes to draw one in about 12 ms, going by how long the last picture in full took, and never fewer than a third each way. A picture already that quick is left whole.
- **Faces** off draws the object as it is, in its parts' colours: no face has a colour of its own and nothing picked out is shown. Drawn by regions, a picture then asks nothing of the one compiled shader, which saves about half a millisecond of the synth's 4.5 (3.7 to 4.3 ms against 4.5 to 5.0; 5.4 to 5.8 against 5.9 to 6.5 on the other model). Drawn by the one compiled shader it changes the colours and not the time. Regions and Faces are kept as they were left, in the browser's own store for the page.
- **Where the work goes:** Gates switches the shader's skipping off, so the time it saves can be read. Cost, Steps and Share draw a map in place of the picture, from a second shader that counts its work (with Regions, from each region's shader, added up over the regions a pixel's ray crosses). Cost is what a pixel's ray ran, in whole evaluations of the model (every operation of the model run once is 1), on a fixed scale of tens, blue under 1 and red over it. It is Steps (how often the ray evaluated the model) times Share (how much of the model an evaluation ran). The picture's totals are under the lists.

It needs a browser with WebGPU: a recent Chrome, Edge or Safari.

### What the maps count, and what they showed

An evaluation is counted in two ways. What the model's own logic needs: an operation counts where its block is reached. And what the graphics card runs: everything inside a gate that is entered, both sides of every select in it. The second is the one the maps show, because it is the one the time follows: on an M1 Max, two assemblies with gates on and off came to 1,050 to 1,850 million counted operations a millisecond, while by the first count the same four pictures differ sixfold. A compiler turns a small branch into both sides and a choice.

| A fitted picture of 1432 x 1536 | Time, gates on / off | Evaluations a pixel | Steps | Share of the model | Its logic needs |
|---|---|---|---|---|---|
| a synth's study, 4,912 nodes, 14,000 operations | 14 / 21 ms | 4.4 | 10.0 | 44% | 16% |
| the same synth built from Onshape, 5,435 nodes, 33,600 operations | 44 / 61 ms | 9.5 | 14.0 | 67% | 9% |

The second model's body had its chamfers as a loft between four copies of its outline, built once for each shell: a third of its operations, none of them skipped, and a slope of 1.44 that shortened every step. Pulled with exact chamfers (`Sketch.extrude ~chamfer_hi`), each shell from the seam to its own face, it is the same solid at 400,000 points, 3,636 nodes and 19,100 written operations, at a slope of 1: 18 to 25 ms where the same picture took 43 to 50, and 5.3 evaluations a pixel (10.5 steps of 51%).

So the gates save a third of the time, and a pixel still costs several whole evaluations. The logic's own need is what skipping at every select would come to, if the graphics card could skip small blocks for nothing.

### What a region's own program would cost

Measured on 25 November 2026 before choosing how to cull further, on the second of those models (the programs that measured it were not kept).

- **A region needs very little of the model.** With the octree's decisions for a cell, an evaluation there needs a mean of 2.2% of the model's operations in cells of 8 mm, 0.7% in cells of 4 mm and 0.2% in cells of 2 mm. Nearly every cell's program is its own: 2,210 different ones in 2,561 cells of 8 mm.
- **A picture marched cell by cell,** passing a cell without surface for nothing and marching one with surface on its own program, comes to 0.31, 0.086 and 0.029 whole evaluations a pixel at those three sizes, where the shader as it is spends 9.5. The number of evaluations hardly changes (14.1 to 10.6 a pixel): what changes is what each one runs.
- **The shader writes a node once for every place that needs it:** 33,600 operations for a model of 13,500.
- **An operation read from a list and carried out costs 25 times a compiled one** on an M1 Max: 60 million a millisecond against 1,500, in a test page where every pixel ran a program of 100 operations 16 times. It is the same whether all pixels share one list or every tile of 8 pixels or more has its own; with a list for every 4 pixels it falls to 42, every 2 to 22, and with a list picked for every pixel to 8.

- **Boxes round the surface, turned to lie along it, do no better than cells.** Each face's surface points got a box on their own axes, cut in two while too thick, mostly empty, or needing too long a program; a ray was marched inside every box it passed. Drawn nearest first with a depth test, 31,154 boxes come to 0.117 whole evaluations a pixel and 46,613 to 0.101, where 16,127 cells of 4 mm come to 0.086; without the order and the test they come to 0.9. The cutting ends with boxes about the size of a cell (4.7 by 3.4 by 0.6 mm), because the kernel decides a select over the ball that holds a region, so a region's program follows how far across it is, not how thin. Nor do boxes save evaluations: 11 a pixel against 13, since a hit costs five (itself and four for the normal) and a ray passes ten boxes, most of them without hitting.

- **The kernel's test is the larger loss, for cells and boxes alike.** Taking a select as decided wherever it goes one way at every point of a lattice in the region (the most any test could decide; a ceiling, since points can miss a place), cells of 8 mm come to 0.057 whole evaluations a pixel and cells of 4 mm to 0.018: five times less than with the test over a ball. Boxes at the same ceiling come to 0.041 with 7,108 boxes and 0.022 with 14,284, nearest first; 0.2 and 0.115 in any order. So with a perfect test boxes match cells and do not pass them.

Put together, these predicted about 180 ns a pixel as it is, and 69, 19 and 6.5 ns interpreted cell by cell at 8, 4 and 2 mm.

### The lists, built: the work is as predicted, the time is not

The cell-by-cell shader was built into the page (a Lists switch; `lib/tape.ml` wrote the lists, and `dune test` held them to the model's field), and taken out again when regions were built. On the second model it drew the same picture and does the work the count predicted: 0.087 whole evaluations a pixel, 10.8 evaluations each of 0.8% of the model, where the compiled shader does 9.5. But the picture takes 36 to 54 ms, the same as compiled (43 ms). The graphics card carries the lists out at 6 million operations a millisecond, not the 60 of the test page.

- With every pixel made to run one and the same list, the rate is 21 million: the shader as it stands is three times slower than the test page's even when all pixels agree. Why is not found. Carrying the lists out in one place of one loop, and keeping the going from cell to cell apart from the evaluating, changed nothing.
- The other factor of three is pixels disagreeing. Which list a pixel is on at a given evaluation makes patches some 10 pixels across in the picture, and the test page had shown what disagreement costs: 8 to 11 million with a list picked for every pixel, with or without a branch on the instruction, and worse the more slots the values have (19 million with 12 slots, 11 with 128).

So the prediction held for the work and failed for the rate.

**Why the rate fails,** from a lab page that carried out the same model's real lists under conditions set one at a time (not kept):

- The interpreter is not at fault. With all pixels on one list it runs at 73 million operations a millisecond, with a list for every tile of 16 pixels at 66, of 8 at 59, of 4 at 33, and with a list for every pixel at 6.
- Pixels are worked in groups of about 8, and a group takes as long as its slowest: with pixels making from 1 to 31 evaluations in place of 16 each, 73 falls to 42. Lists of different lengths in one group cost the same way.
- A list for every pixel is slow however the interpreter is written. Five were tried on lists of one length: a switch over an array of values; every result worked out and one chosen; the values in plain variables with no array; the list read from a texture; and values chosen by masking bits, with nothing a compiler could make a branch of. With 8 slots all five run at 49 to 63 million with one list and 17 to 19 with a list a pixel (9 to 13 with 64 slots). Only the last keeps its rate with a list for every tile of 4 pixels.

So this graphics card is slow wherever neighbouring pixels read different data, and cells of 4 mm make patches of about 10 pixels. What the lists save in work, a shader that reads them cannot collect.

### The same short programs, compiled: measured, not built

A shader cannot be compiled for each cell (4,828 different programs in 11,356 cells of 4 mm, on the second model after its body was rebuilt). But cells whose programs are alike can share one that serves them all, and each cell be drawn as a box with its cluster's shader, marched inside the box. Measured for that, with the cells sorted greedily into clusters (each program put where the whole grows least):

| Clusters | An evaluation runs | All the programs together | A pixel's work, up to its hit | With the cells behind the hit |
|---|---|---|---|---|
| 64 | 17.3% of the model | 5.3 times the model | 1.45 whole evaluations | 6.9 |
| 256 | 4.8% | 8.9 times | 0.40 | 2.0 |
| 1,024 | 1.9% | 17.3 times | 0.16 | 0.81 |

The compiled shader as it is spends 13.2 in the same unit (5.3 evaluations of a text that writes the model's 7,694 operations as 19,141). A ray makes 8.4 evaluations up to its hit and 40 if every cell behind the hit is marched too.

- **The cells behind the hit are not free.** Eight sheets over a picture drawn nearest first, each pixel doing the same heavy work, take one sheet's time with a plain shader, and seven times that with a shader that may throw its pixel away or that says its own depth, which a cell's shader must. So it is the last column that counts, unless the page finds the hidden cells itself (a first pass a quarter as fine, say).
- **Many small shaders compile faster than one large one:** 256 of 270 operations in 0.5 s and 1,024 of 130 in 1.0 s, where one of 19,000 takes 1.8 s.
- **The sorting took 30 s for 256 clusters** as it was written for the measurement. It would stand between a save and the picture, so it has to be found a way of doing in well under a second.

By these numbers: 6.5 times less work than now with 256 clusters and 16 times with 1,024, at the rate compiled code is known to run at in real pictures; and 33 and 82 times if the hidden cells were spared.

### Regions, built: compiled code keeps its rate

Built on 25 November 2026 as the Regions switch (`lib/region.ml` cuts the model and writes the shaders; `dune test` holds each region's program to the model's field in its box). The cut is not the clusters measured above but the simplest one: the octree's cells, the costliest cut in eight until there are enough, each with the program the kernel's one test of the whole cell leaves. A region is drawn as its box, far sides only, and a pixel of the box is a ray marched from where it comes into the box to where it leaves; the nearest hit is kept by depth. A ray that comes into a region inside the solid stops after one evaluation: it met the surface in a region before.

A picture is two passes. The regions find each pixel's hit: how far along its ray, and which way the surface faces. Then the one compiled shader is evaluated once at each hit, for the face and the feature, and colours the pixel. As it now stands, same camera, a picture of 1432 by 1536 on an M1 Max, the two ways timed in turn in one page:

| | The one compiled shader | 512 regions, then the face | Pixels of the model that differ by more than 8 parts in 255 |
|---|---|---|---|
| the synth's assembly (3,636 nodes) | 21.4 to 22.5 ms | 4.3 to 4.6 ms | 1,175 of 281,374, on edges and the outline |
| the older packaging study (4,912 nodes) | 12.3 to 13.9 ms | 5.6 to 7.5 ms | 1,541 of 334,954 |

**As first built,** a region's shader chose the face too, at every evaluation. On the synth's assembly (9,275 operations written straight through, a choice counted as one), the same size of picture:

| Regions asked for | Regions, shaders | An evaluation runs | Evaluations a pixel | Whole evaluations a pixel | A picture | Ready after |
|---|---|---|---|---|---|---|
| the one compiled shader | | | 10.5 | 10.9 | 20 to 24 ms | |
| 64 | 68, 68 | 27% of the model | | | 61 to 72 ms | 1.7 s |
| 256 | 258, 258 | 12.7% | | | 10 to 11 ms | 6.4 s |
| 512 | 514, 496 | 6.9% | 22.1 | 1.53 | 4.8 to 5.2 ms | 3.2 s |
| 1,024 | 1,030, 932 | 4.8% | 23.2 | 1.12 | 3.7 to 6.8 ms | 3.9 s |
| 2,048 | 2,048, 1,823 | 2.6% | 24.4 | 0.65 | 3.1 to 5.6 ms | 5.5 s |

- **The rate holds:** 1,500 million operations a millisecond with 1,024 regions, what compiled code runs at in the one large shader. Nothing is read to decide what to run, and pixels worked together are nearly always in one box.
- **The picture is the same.** Of 281,374 pixels of the model, 7,383 differ at all from the compiled shader's and 1,176 by more than 8 parts in 255, every one of those on an edge or the outline; 8 pixels of the outline are missing and 9 are added.
- **Ready** is the kernel's cut (0.4 to 1.3 s: it writes 7 to 22 MB of shaders) and then compiling them. Long shaders compile much slower than short ones, which is why 256 regions take longer than 1,024. The page draws with the one compiled shader until the regions are there, so a picture is not later than before.
- **A ray crosses regions behind the one it hits:** 22 evaluations a pixel where the one shader makes 10.5 (27 and 10 on the other model). Nothing spares them yet.
- **The programs are longer than they need be.** For the same cut, measured and not built: taking a select as decided where every cell of the finest level in the region decides it the same way gives 3.0% in place of 4.5% with 1,024 regions (7.4% for 12.7% with 256), and the programs still hold all over the box. Over the finest cells that hold surface alone, 2.4% (5.7%), but then each of those 11,356 cells must be its own box. And letting a cell that never reaches a select have no say in it, 1.7% (3.2%), with all the programs together 14.6 times the model in place of 37.9 (7.3 for 26.4); that needs each cell's own choices handed to the shader as data. The last would make 256 regions do what 1,024 do now, with a fifth of the text to compile.
- **On the other large model that first build did not pay, and the cause was not the one first given.** The older packaging study (4,912 nodes, 9,483 operations), same camera and picture size: 11.4 to 12.1 ms by the one compiled shader, 30.7 to 31.1 ms by 513 regions, with the same picture. Its regions' programs are longer (12.3% of the model an evaluation, all the shaders together 59 times the model), but that is not why: the synth cut into 258 regions has programs as long (12.4%), does nearly as much work a picture (14.2 thousand million operations against 18.2; 22.0 evaluations a pixel against 26.6) and takes 11.1 ms. What takes the time is **choosing the face and the feature at every evaluation**, two selects for every choice left in the program. With those lines out of the shaders the same picture's rays take 6.2 ms; on the synth they cost next to nothing (5.8 ms without, 6.4 with). Why the two models differ so is not found. Ruled out, each by a picture timed: that the numbers are integers (as floats, 39 ms where it was 39), that the selects become branches (chosen by arithmetic instead, 65 ms), and that they were written after all the operations (written after each choice, 39 ms).
- **So the face is found once, at the hit.** Tried first as a second copy of its program in each region's shader, used once after the march: 7.1 ms on that model and 6.8 on the synth, but five times slower to compile (37 s for 7, 22 s for 5). Built instead as the pass described above, which needs no second copy and leaves the regions' shaders a quarter shorter.
- **The shorter programs matter more on that model:** for its 513 regions, 5.6% in place of 12.1% with every finest cell asked, 3.8% with the finest surface cells alone, 2.1% with each cell's own choices as data; all programs together 28, 18 and 10 times the model in place of 59.
- **Times read in one sitting differ from another's** by a third or more (the synth's compiled shader 20.8 ms, then 23.9, then 29.9 with the card busy compiling). Each comparison above is of pictures timed in one page, one after the other. Times to be ready are rougher still: 3 to 17 s were read for the same 512 regions, from a page that was mostly not on screen, where the browser gives it less of the machine. With the face out of the regions' shaders and the page hidden, the regions were ready within about 3 s on the synth and 5 s on the other model, read to the second.
- **Not looked at:** the eye inside the solid (the one shader marches out from within; regions past the eye's own draw nothing); a feature shown alone; any other graphics card.

## How the parts meet

Each part has one job. The command (`cli/`, OCaml) builds, measures and draws pictures, and exits. The page (`web/view.html`) is the window, and holds no model. Between them a small server (`mk52 serve`, started by `mk52 open`) stays for as long as the page is wanted: it listens on this machine only, and does no more than hand the page what is in the design folder's `_mk52/` and run the command for what the page needs worked out. So a kernel built again is the kernel the next click gets, and the page is read from the tool each time it is loaded: neither needs the server started again.

| In `_mk52/` | Written by | What it is |
|---|---|---|
| `open/<project>.functions` | the command | a project's functions that can be built alone |
| `open/<model>.model` | the command | a model as last built; it is open in the page for as long as this file is there |
| `open/<model>.edges.json` | the command | its edges and corners, if extracted |
| `open/<model>.<feature>.gates` | the command | where its shader's gates are entered |
| `assets/<number>.mesh` | the command | what a model shows from outside, as triangles: one a file, by a number made of its path |
| `front` | the command | the model the page is to bring to the front |
| `state/<model>.json` | the server, from the page | what is picked out in it and where the camera is, for an agent |
| `server` | the server | its process and its port (8752, or the next one free) |
| `seen` | the server | written while a page is looking, so `mk52 view` opens one only when none is |

The server looks at the design's code a few times a second, and runs `mk52 refresh` when a file was saved. The page asks it what is in the folder as often. A model written: the page asks for `mk52 scene <model>` (how it is drawn: box, shader, features, sketches) and compiles the shader; with Regions on, then for `mk52 regions` too (the cut, and a shader for each region). A click: `mk52 pick`.

## What is in it

| File | Lines | What it holds |
|---|---|---|
| `lib/expr.ml`, `graph.ml`, `solid.ml`, `frame.ml` | 367 | formulas; the graph of pieces, applies and selects, with features; primitives as true distances; frames |
| `lib/sketch.ml` | 380 | a closed outline of lines and arcs as its exact distance, pulled (with exact chamfers), turned or lofted into a solid |
| `lib/spline.ml`, `fit.ml` | 312 | free-form faces: a B-spline height patch, and fitted cubic patches |
| `lib/prog.ml` | 290 | the graph compiled to arrays, and the one evaluator |
| `lib/octree.ml` | 132 | top-down pruning by slope bound: every cell with its decisions |
| `lib/extract.ml` | 674 | edges and corners: seed, walk, cut, assemble, flag |
| `lib/surface.ml` | 697 | a triangle mesh of the surface, attached to its creases and corners |
| `lib/dxf.ml` | 162 | a flat part's outline, traced, as the lines, arcs and circles it is, in a DXF file |
| `lib/shader.ml` | 240 | the compiled program as a WGSL function, its chains in gates, and the table of where each gate is entered |
| `lib/step.ml`, `mesh.ml` | 804 | a STEP file read (planes, cylinders, cones, spheres, tori, NURBS; assemblies), and its faces cut into triangles: mk49's, as they were |
| `lib/asset.ml` | 117 | something made elsewhere shown with a model: where a model's code puts it, and its triangles from a STEP or an STL file |
| `lib/region.ml` | 147 | the model cut into regions, and each region's own short program (the field alone) as a WGSL function |
| `lib/render.ml`, `png.ml` | 207 | a picture made by the kernel, as a PNG |
| `lib/emit.ml`, `place.ml`, `json.ml` | 215 | a model as a file; where things are kept; JSON |
| `cli/main.ml` | 228 | the `mk52` command |
| `cli/project.ml` | 316 | models by name, built: a project compiled once and any of its functions built from it |
| `cli/scene.ml` | 128 | what the page asks: how a model is drawn, its regions, what a ray hits |
| `cli/serve.ml` | 209 | the page's server: the folder's files, the command run, the code watched |
| `cli/mcp.ml` | 145 | tools for an agent: `picture`, `kernel_probe`, `kernel_extract`, `viewer_state`, `feature_brief` |
| `web/view.html` | 1174 | the page: its shaders (the march, a region's box, lines and dots, the counting maps), the drawing, the camera, the lists, the mouse |
| `models/`, `test/`, `experiments/`, `bench/` | | the kernel's test shapes, its 220 checks (`dune test`, 15 s), and mk49's experiments |

## Not there

- **A STEP file is shown, not compared with:** no reference, no deviation, no check against faces, no fitting. That stays in `pointer_mk49`.
- **An asset is not in the kernel's own pictures, cannot be clicked, and is not checked against the model.** Whether one runs into the model has to be seen.
- **The mesh is not drawn in the page.** `mk52 mesh` writes the STL file and a picture.
- **A native app.** The first mk52 had one, in Swift with Metal. WebGPU drew the same shader as fast (a synth's assembly of 4,912 nodes at 2200 x 1800 on an M1 Max: 14.7, 41.3 and 85.9 ms with the whole model in view, filling the view and close up, against 14.8, 42.3 and 86.1 ms natively), and a page can be driven and checked by an agent, which a native window cannot.
- **Frame times are measured on that one model only,** in a test page that drew pictures one straight after another. The page's own figure for a single picture after a pause has read higher (23 ms for the first of those three views); why is not yet known.
- **A large shader takes a second or more to compile** (1.3 s for that assembly). The page keeps showing the model as it was until the new shader is ready.
- **Tried in one browser only:** Chromium, on a Mac. Nothing tests the page by itself; it was driven by hand through its lists, the mouse, picking, Find edges and a rebuild on save.

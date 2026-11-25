(* Circuit boards, written as code like any other part.

   A solid is a function of space, below zero inside. Copper is the same thing in the
   board's plane: a pad, a stretch of trace and a via are each the distance to their
   outline. So what the kernel does with a solid it does with copper: a trace is a
   sketch line given a width, the clearance between two nets is the clearance between
   two solids, and a pour is one formula, the board less every other net's copper
   grown by the clearance.

     let b = Circuit.Board.make "lights" (Circuit.Board.rect 30. 20.) in
     Circuit.Board.part b "R1" ~footprint:Circuit.Footprint.r0603 (14., 12.);
     Circuit.Board.net b "VCC" [ "J1.1"; "R1.1" ];
     Circuit.Board.trace b ~net:"VCC" [ Circuit.Board.pin b "J1.1"; Circuit.Board.pin b "R1.1" ];
     Circuit.Board.solid b

   The last line is the board as a solid, a plate with its holes, to be a part of an
   assembly; the board itself goes with whatever model it is built in, and mk52 board
   <model> checks it, draws it and opens it in a page.

   Each of these is a file of its own, circuit_<name>.ml: under one name here so that
   none of them is taken for a file of a project that has opened Mk52. *)
module Copper = Circuit_copper
module Footprint = Circuit_footprint
module Board = Circuit_board
module Pour = Circuit_pour
module Check = Circuit_check
module Draw = Circuit_draw
module Page = Circuit_page

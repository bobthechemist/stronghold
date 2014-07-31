BeginPackage["stronghold`"]

initGame::usage = "initGame[<lcdlink>] initializes the game.\
  <lcdlink> must be the file path to the MathLink code that\
  interfaces with the Adafruit RGB LCD panel."
startGame::usage = "startGame[] starts a new game.  At this point\
  the current cell will continue to evaluate until the player\
  wins or quits.  Presently, presing SELECT on the lcd panel\
  will cause the game to quit."
closeGame::usage = "closeGame[<lcdlink>] will release control\
  of the lcd panel and clean up any symbols that might have\
  leaked through."


Begin["`Private`"]

(* ==initGame== *)
(* Loads the i2c driver needed for lcdlink and installs the lcdlink  *)
(*  package. Note that all calls to the lcdlink package must be      *)
(*  explicit; at the moment I do not know why the package does not   *)
(*  get placed into the current packages context.  Not even sure if  *)
(*  this is a bad thing.                                             *)

  Clear[initGame];
  initGame[panellink_:"/home/pi/mywiringPi/rpi-lcdlink/lcdlink"]:=Module[{},
    <<"!gpio load i2c";
    link = Install[panellink];

    (* Welcome screen *)
    lcdlink`lcdClear[];
    lcdlink`lcdPuts[0,0,"   Welcome to:"];
    lcdlink`lcdPuts[0,1,"   Stronghold"];
    Pause[1];
    lcdlink`lcdClear[];
    lcdlink`lcdPuts[0,0,"     of the"];
    lcdlink`lcdPuts[0,1," Dwarven Lords."];

    (* Generate some useful symbols and rules *)

    numrules = {1->"NORTH", 2->"EAST", 3->"SOUTH", 4->"WEST"};
    dirrules = {"UP"->"NORTH", "RIGHT"->"EAST", "DOWN"->"SOUTH",
      "LEFT"->"WEST"};
    posrules = {"NORTH"->{-1,0},"EAST"->{0,1}, "SOUTH"->{1,0},
      "WEST"->{0,-1}, "UP"->{-1,0}, "RIGHT"->{0,1},
      "DOWN"->{1,0}, "LEFT"->{0,-1}};
    blank = "            ";
    assoc=Association[{"SELECT"->16,"LEFT"->8,
      "UP"->4,"RIGHT"->2,"DOWN"->1}];
    buttons ={};
  ]

(* ==startGame== *)
(* Creates the maze and assigns the position of the treasure and the *)
(*   player.  Then executes the main game loop.                      *)

  Clear[startGame];
  startGame[]:= Module[{},
    (* Create a maze that fits in a 4x2 block on the lcd screen *)
    (*  makeMaze can only make odd-numbered rows and columns so *)
    (*  pad the maze after making it.                           *)

    maze = ArrayPad[makeMaze[19,15,0.75,0.75],{{0,1},{0,1}}];
    (* Zeros in the maze matrix are hallways, so find a hallway *)
    (*  spot to place the player and treasure.  Note how the    *)
    (*  maze is temporarily modified to avoid starting the      *)
    (*  player at the same spot as the treasure.                *)

    cpos = RandomChoice[Position[Normal[maze[[1;;-2,1;;-2]]],0]];
    tpos = RandomChoice[Position[Normal[
      ReplacePart[maze,cpos->1]][[1;;-2,1;;-2]],0]];

    (* the main loop *)
    While[True,
      lcdlink`lcdPuts[0,0,blank];
      lcdlink`lcdPuts[0,1,blank];
      printquad[maze,whereami[maze,cpos][[1]]];
      blinkmap[maze,cpos,5];
      options = Flatten@Position[whereami[maze,cpos][[3]],False]
        /. numrules;
      lcdlink`lcdPuts[0,0,"Runestone"];
      lcdlink`lcdPutc[10,0,runestone[cpos,tpos]];
      lcdlink`lcdPuts[0,1,blank]; 
      lcdlink`lcdPuts[0,1,ToString[StringTake[#,1]&/@options]];
      buttons = buttoncheck[];
      If[MemberQ[buttons, "SELECT"],
        lcdlink`lcdPuts[0,0,blank];lcdlink`lcdPuts[0,0, "Bye!"];
          Break[]
      ];
      If[!MemberQ[options, First@buttons/. dirrules],
        lcdlink`lcdPuts[0,0,"You hit the"];
        lcdlink`lcdPuts[0,1,"wall, dummy."];
        Pause[1];, cpos += First@buttons/.posrules];
      If[cpos == tpos,
        lcdlink`lcdClear[];
        lcdlink`lcdPuts[0,0," You found the"];
        lcdlink`lcdPuts[0,1,"   treasure!"];
        Pause[2];
        Break[];
      ]
    ] 
  ];  

  Clear[closeGame];
  closeGame[panellink_:"/home/pi/mywiringPi/rpi-lcdlink/lcdlink"]:=
    Module[{},
    Clear[maze,cpos,tpos,blank,numrules,dirrules, posrules,
      assoc, buttons];
    Uninstall@panellink;
  ]

  Clear[makeMaze]
  makeMaze[width_,height_,complexity_,density_]:=Module[
    {w = width-2,h = height-2, c = complexity, 
    d = density,m,shape,x,y,$x,$y,n},
    (* Only dealing with odd dimension lengths *)
    shape = If[OddQ@#,#,#+1]&/@{h,w};
    (* Convert complexity and density into shape units *)
    c = IntegerPart[c(5 Total@shape)];
    d = IntegerPart[d*Times@@(shape/2)];
    (* Create empty maze with a border *)
    m = SparseArray[{{1,_}->0,{h,_}->0,{_,1}->0,{_,w}->0},{h,w}];
    (* Start drawing maze *)
    Table[(
      (* Place a wall, can only place them in at most *)
      (* 50 percent  of the available spots *)
      {y,x}=RandomInteger[{1,IntegerPart[#/2]}]*2&/@shape;
      m[[y,x]]=1;
      (* Create aisles *)
      Table[(
        n = {};
        (* Find neighbors, accounting for the edge *)
        If[x>2,AppendTo[n,{y,x-2}]];
        If[x<shape[[2]]-1,AppendTo[n,{y,x+2}]];
        If[y>2,AppendTo[n,{y-2,x}]];
        If[y<shape[[1]]-1,AppendTo[n,{y+2,x}]];
        (* Choose a neighbor and make an aisle in that direction *)
        {$y,$x}=RandomChoice[n];
        If[m[[$y,$x]]==0,m[[$y,$x]]=1;
        m[[$y+(y-$y)/2,$x+(x-$x)/2]]=1;
        {y,x} = {$y,$x};]
      );,{c}];
    );,{d}];
    ArrayPad[m,1,1]
  ]


(*Print map quartile*)
  Clear[printquad];
  printquad[m_,q_,w_:3]:=Module[{chars = {}},
    (* Currently defaults to print all quartiles
      of the map in the 4th quartile *)

    (* create characters *)
    chars = Table[mat2char[m,0,4q+i],{i,0,3}]~Join~
      Table[mat2char[m,1,4q+i],{i,0,3}];
    (* define characters *)
    Table[lcdlink`lcdCharDef[i,chars[[i+1]]],{i,0,7}];
    (* print characters *)
    Table[(
      lcdlink`lcdPutc[4w +i,0,i];lcdlink`lcdPutc[4w +i,1,i+4];),
      {i,0,3}];
  ];

(*Extract the top/bottom row of the maze matrix*)
  Clear[mat2row];
  mat2row[m_,r_Integer]:=FromDigits[#,2]&/@
    Normal@maze[[8 r+1;;8r +8,5#+1;;5#+5]][[All]]&/@Range[0,15]

(*Other general screen scrolling functions*)
  Clear[makefsrow]; 
  makefsrow[i_Image,r_Integer]:=Table[2^Range[4,0,-1].#&
    /@(1-ImageData@ImageTake[i,{8r +1,8r+8},{5x+1,5x+5}]),{x,0,15}]

  scroll[p_,r_,b_]:=Module[{},
    lcdPutc[p-4+If[p<4,16,0],r,32];
    lcdCharDef[Mod[p,4]+If[r==1,4,0],b[[p+1]]];
    lcdPutc[p,r,Mod[p,4]+If[r==1,4,0]];
  ]

  scrollimage[top_,bottom_,num_:5,color_:4]:=Module[{},
    lcdClear[];
    lcdColor[color];
    Table[scroll[Mod[i,16],0,top];
    scroll[Mod[i,16],1,bottom];,{i,0,num 16-1}];
  ]

(*Extract a particular character from the maze matrix*)
  Clear[mat2char];
  mat2char[m_,r_Integer, c_Integer]:=FromDigits[#,2]&/@
    Normal@m[[8 r+1;;8r +8,5c+1;;5c+5]][[All]]

(*Determine if position is a wall (or outside the boundary due to padding)*)
  iswall[m_,pos_]:=If[Or[m[[First@pos,Last@pos]]==1,
    First@pos>15,Last@pos>79],True,False]

(*Determine player's quadrant, character (row, col) 
  and wall locations (N, E, S, W)*)
  Clear[whereami]
  whereami[m_,pos_]:=Module[{ret = {}},
    (* which quadrant *)
    AppendTo[ret,IntegerPart[Last@pos/20]];
    (* which character: row then col*)
    AppendTo[ret,{IntegerPart[(First@pos-1)/8],
      IntegerPart[(Last@pos-1)/5]}];
    (* Where are the walls? N, E, S, W *)
    AppendTo[ret,iswall[m,pos+#]&/@{{-1,0},{0,1},{1,0},{0,-1}}];
    ret
  ];

(*create map with blinking caracter location.
  Need to know which quad to print and which char to redefine*)
  Clear[blinkmap]
  blinkmap[m_,pos_,num_:3]:=Module[{info = whereami[m,pos],
    b,u,w=whichchardef[pos]},
    (* print the appropriate part of the map *)
    printquad[m,info[[1]]];
    (* Store chardefs for blinked (b) and unblinked (u) characters *)
    u = mat2char[m,info[[2,1]],info[[2,2]]];
    b = mat2char[ReplacePart[m,pos->1],info[[2,1]],info[[2,2]]];
    (* Blink for a bit *)
    Table[(
      lcdlink`lcdCharDef[w,b];Pause[0.2];
      lcdlink`lcdCharDef[w,u];Pause[0.2];),
      {num}
    ];
  ]

(*Find which chardef is used to write a character 
  containing a given position*)
  Clear[whichchardef]
  whichchardef[pos_]:=IntegerPart[Mod[#[[2]]-1,20]/5]+
    4*IntegerPart[(#[[1]]-1)/8]&[pos]

(* Loop that waits for a button press and returns the result *)
  buttoncheck[]:=Module[{buttons = {}},
    While[buttons == {},
      buttons = DeleteCases[BitAnd[FromDigits[
        lcdlink`lcdButtonStatus[],2],assoc],0]//Keys;];
    buttons
  ]

  Clear[waitforbuttons];
  waitforbuttons[]:=Module[{},While[True,If[buttons!={},Break[];]];buttons]

(*Response from the dwarven runestone*)
  Clear[runestone];
  runestone[cpos_,tpos_]:=Module[{deg},
    deg = Round[ArcTan[Sequence@@(tpos-cpos)]/Degree//N];
    Round[(deg+180)/360*12]+165
  ];

End[]
EndPackage[]

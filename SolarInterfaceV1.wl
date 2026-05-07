(* ::Package:: *)

BeginPackage["SolarInterfaceV1`", {"SolarCoreV1`"}]

StartApp::usage = "Lancia l'interfaccia con zoom ottico reale e indicatore di focus."

Begin["`Private`"]

StartApp[] := DynamicModule[{dati = GetSystemData[], pianeti},
  pianeti = Keys[dati];
  
  Manipulate[
   Module[{pos, targetPos},
    
    (* 1. Calcola la posizione di tutti i pianeti *)
    pos = AssociationMap[
      Function[p, 
       If[dati[p, "Dist"] == 0, {0, 0, 0}, 
        dati[p, "Dist"] * {Cos[t * dati[p, "Speed"]], Sin[t * dati[p, "Speed"]], 0}]
      ], pianeti];
      
    (* 2. Posizione dell'oggetto selezionato *)
    targetPos = pos[selezione];
    
    Graphics3D[{
      (* Orbite *)
      If[trails, {Gray, Opacity[0.3], 
        Table[Line[Table[{r*Cos[a], r*Sin[a], 0}, {a, 0, 2 Pi, 0.1}]], 
          {r, {4, 7, 10, 15, 25, 35, 45, 55}}]}, {}],
          
      (* Pianeti, Sole e ALONE DI SELEZIONE *)
      Table[{
        (* HIGHLIGHT: Sfera semitrasparente attorno al pianeta in focus *)
        If[p == selezione, 
           {White, Opacity[0.25], Glow[White], Sphere[pos[p], dati[p, "Size"] * 1.6]}, 
           {}
        ],
        
        (* Il corpo del pianeta *)
        {If[usaTexture && dati[p, "Texture"] =!= None, Texture[dati[p, "Texture"]], dati[p, "Color"]],
         Sphere[pos[p], dati[p, "Size"]]}
       }, {p, pianeti}]
     },
     
     Background -> Black, 
     Boxed -> False, 
     Lighting -> "Neutral", 
     SphericalRegion -> True,
     
     (* --------------------------------------------------------- *)
     (* LA VERA SOLUZIONE AL CLIPPING: IL METODO "TELESCOPIO" *)
     PlotRange -> {{-60, 60}, {-60, 60}, {-60, 60}},
     ViewCenter -> {(targetPos[[1]] + 60)/120, (targetPos[[2]] + 60)/120, 0.5},
     ViewAngle -> (35 * Degree) / zoom,
     (* --------------------------------------------------------- *)
        
     ImageSize -> {700, 500}
    ]
   ],
   
   (* Controlli *)
   Style["Navigazione", Bold, 12],
   {{selezione, "Sole", "Focus su"}, pianeti, ControlType -> PopupMenu},
   {{zoom, 1, "Livello Zoom"}, 1, 10, Appearance -> "Labeled"},
   
   Delimiter,
   Style["Informazioni sul pianeta", Bold, 12],
   Dynamic @ Panel[
     Style[dati[selezione, "Info"], "Text", 11], 
     ImageSize -> {200, 150}, Alignment -> {Center, Top}
   ],
   
   Delimiter,
   Style["Simulazione", Bold, 12],
   {{t, 0, "Tempo trascorso"}, 0, 50, ImageSize -> Small, AnimationRate -> 0.5, AnimationRepetitions -> Infinity, AnimationRunning -> False, ControlType -> Animator},
   {{trails, True, "Mostra orbite"}, {True, False}},
   {{usaTexture, False, "Usa Texture"}, {True, False}},
   
   ControlPlacement -> Left,
   TrackedSymbols :> {t, zoom, selezione, trails, usaTexture}
  ]
]

End[]
EndPackage[]

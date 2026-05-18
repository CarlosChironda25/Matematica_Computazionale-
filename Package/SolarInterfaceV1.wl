(* ::Package:: *)

(*Definizione del pacchetto ed importazione di SolarCoreV1*)
BeginPackage["SolarInterfaceV1`", {"SolarCoreV1`"}]

StartApp::usage = "Lancia l'interfaccia avanzata ottimizzata per alte prestazioni."

Begin["`Private`"]

StartApp[] := DynamicModule[{
    dati = GetSystemData[], 
    stelleData = GetStarField[], 
    pianeti, pianetaSegreto, punteggio = 0,
    orbitePrecalcolate
  },
  
  pianeti = Keys[dati];
  pianetaSegreto = RandomChoice[pianeti];
  
  (* Costruiamo i cerchi delle orbite solo all'avvio *)
  orbitePrecalcolate = Table[
    Line[Table[{r*Cos[a], r*Sin[a], 0}, {a, 0, 2 Pi, Pi/50}]], 
    {r, {4, 7, 10, 15, 25, 35, 45, 55}}
  ];
  
  Manipulate[
   With[{
     (* Calcoliamo dinamicamente solo la posizione del pianeta in focus *)
     targetPos = If[dati[selezione, "Dist"] == 0, {0, 0, 0}, 
       dati[selezione, "Dist"] * {Cos[t * dati[selezione, "Speed"]], Sin[t * dati[selezione, "Speed"]], 0}]
    },
    
    Graphics3D[{
      (* SFONDO STELLATO *)
      {stelleData},
      
      (* ORBITE STATICHE PRE-CALCOLATE *)
      If[trails, {Gray, Opacity[0.3], orbitePrecalcolate}, {}],
          
      (* PIANETI *)
      Table[
       With[{currPos = If[dati[p, "Dist"] == 0, {0, 0, 0}, 
         dati[p, "Dist"] * {Cos[t * dati[p, "Speed"]], Sin[t * dati[p, "Speed"]], 0}]},
        {
         (* Sfera intorno al pianeta selezionato *)
         If[p == selezione, {White, Opacity[0.25], Glow[White], Sphere[currPos, dati[p, "Size"] * 1.6]}, {}],
         
         (* Rendering pianeta *)
         {If[usaTexture && dati[p, "Texture"] =!= None, Texture[dati[p, "Texture"]], dati[p, "Color"]],
          Sphere[currPos, dati[p, "Size"]]}
        }
       ], {p, pianeti}]
     },
     
     Background -> Black, Boxed -> False, Lighting -> "Neutral", 
     SphericalRegion -> False,
     PlotRange -> {{-70, 70}, {-70, 70}, {-70, 70}},
   
     ViewVector -> {
       targetPos + {0, -20, 15},  (* Posizione camera *)
       targetPos                   (* Punto guardato *)
     },
     ViewAngle -> Dynamic[zoom],
     ImageSize -> {700, 700},
     
     PerformanceGoal -> "Speed"
    ]
   ],
   
   Style["Cosa vuoi fare?", Bold, 14, Orange],
   {{modalitaGioco, False, ""}, {False -> "\|01f52d Esplorazione", True -> "\|01f3ae Indovinelli"}, ControlType -> SetterBar},
   
   Spacer[200],
   Delimiter,Style["Controlli simulazione", Bold, 12],
   Spacer[200],
   
   {{selezione, "Sole", "Fai focus su: "}, pianeti, ControlType -> PopupMenu},
   {{zoom, 1, "\|01f50d Zoom"}, 2.5, .5, Appearance -> "Labeled"},
   {{t, 0, "Tempo"}, 0, 50, ImageSize -> Small, AnimationRate -> 0.5, AnimationRunning -> False, ControlType -> Animator},
   {{trails, True, "Mostra orbite: "}, {True, False}},
   {{usaTexture, False, "Usa colori reali: "}, {True, False}},

   Spacer[200],   
   Delimiter,
   Spacer[200],
   
   (* Modalit\[AGrave] gioco *)
   Dynamic @ If[modalitaGioco,
     Column[{
       Row[{
         Style["Stelle: " <> StringJoin[Table["\:2b50\:fe0f", {punteggio}]], 16], Spacer[15],
         If[punteggio > 0, Button["\|01f504 Azzera", punteggio = 0; pianetaSegreto = RandomChoice[pianeti]; selezione = "Sole";, ImageSize -> Automatic], ""]
       }, Alignment -> Center],
       Spacer[5], Style["Indovina chi sono:", Bold, 14, Yellow], Spacer[5],
       Panel[Style[dati[pianetaSegreto, "Quiz"], 14, Italic], ImageSize -> {250, Automatic}, Background -> DarkGray],
       Spacer[10],
       If[selezione == pianetaSegreto,
         Column[{
           Style["\|01f389 Esatto! \[CapitalEGrave] " <> ToUpperCase[pianetaSegreto] <> "!", Bold, 14, Darker[Green]], Spacer[5],
           Button["Prendi la \:2b50\:fe0f e continua!", punteggio++; pianetaSegreto = RandomChoice[DeleteCases[pianeti, pianetaSegreto]]; selezione = "Sole";, BaseStyle -> {Bold, 12}, ImageSize -> 200]
         }, Alignment -> Center],
         Style["Usa il menu sopra per scegliere il pianeta!", 12, Gray]
       ]
     }, Alignment -> Center],
     Column[{
       Style["Info su:" selezione, Bold, 12, LightBlue],
       Panel[Style[dati[selezione, "Info"], "Text", 12], ImageSize -> {250, 150}, Alignment -> {Left, Top}]
     }]
   ],   
   
   ControlPlacement -> Left, TrackedSymbols :> {t, zoom, selezione, trails, usaTexture, modalitaGioco, pianetaSegreto, punteggio}
  ]
]

End[]
EndPackage[]




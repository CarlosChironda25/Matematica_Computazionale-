(* ::Package:: *)

distances = {.29, .38, .56, .85, 1.3, 1.8, 2.2, 2.8};
distances = {.29, .38, .56, .85, 1.3, 1.8, 2.2, 2.8};
planetdistances = {.0165, .025, .041, .029, .075, .043, .033, .026};
planetColors = {LightGray, Orange, Blue, Red, Brown, LightOrange, LightBlue, Darker[Cyan, 0.5]};

planetInfo = {
   "Diametro: 4,879 km\nDistanza dal Sole: 57.9M km\nPeriodo di rivoluzione: 88 giorni\nTemperatura: -180\[Degree]C a 430\[Degree]C\nLune: 0",
   "Diametro: 12,104 km\nDistanza dal Sole: 108.2M km\nPeriodo di rivoluzione: 225 giorni\nTemperatura: 462\[Degree]C\nLune: 0",
   "Diametro: 12,742 km\nDistanza dal Sole: 149.6M km\nPeriodo di rivoluzione: 365 giorni\nTemperatura: -89\[Degree]C a 57\[Degree]C\nLune: 1",
   "Diametro: 6,779 km\nDistanza dal Sole: 227.9M km\nPeriodo di rivoluzione: 687 giorni\nTemperatura: -140\[Degree]C a 20\[Degree]C\nLune: 2",
   "Diametro: 139,820 km\nDistanza dal Sole: 778.6M km\nPeriodo di rivoluzione: 12 anni\nTemperatura: -108\[Degree]C\nLune: 95",
   "Diametro: 116,460 km\nDistanza dal Sole: 1.434B km\nPeriodo di rivoluzione: 29 anni\nTemperatura: -139\[Degree]C\nLune: 146",
   "Diametro: 50,724 km\nDistanza dal Sole: 2.871B km\nPeriodo di rivoluzione: 84 anni\nTemperatura: -197\[Degree]C\nLune: 27",
   "Diametro: 49,244 km\nDistanza dal Sole: 4.495B km\nPeriodo di rivoluzione: 165 anni\nTemperatura: -201\[Degree]C\nLune: 16"
};

Manipulate[
 DynamicModule[{targetPos, camPos},
 
  targetPos = If[selectedPlanet == 0, (*Calcolo la posizione del pianeta selezionato*)
    {0, 0, 0},
    distances[[selectedPlanet]]*{Cos[t/distances[[selectedPlanet]]^(3/2)], Sin[t/distances[[selectedPlanet]]^(3/2)], 0}
  ];
 
  camPos = targetPos + {0, -10, 4}; (*Per muovere la telecamera insieme al pianeta*)

  Graphics3D[
   {Yellow, {Sphere[{0, 0, 0}, .16]},
    {FaceForm[], Sphere[{0, 0, 0}, Last[distances]]}, Gray,
    If[trails,
     Table[{Opacity[0.125],
       Line[Table[distances[[i]]*{Cos[\[Theta]], Sin[\[Theta]], 0}, {\[Theta], 0, 2 Pi, 2 Pi/100.}]]
      }, {i, 8}], {}
    ],
    Table[{planetColors[[i]],
      Sphere[distances[[i]]*{Cos[t/distances[[i]]^(3/2)], Sin[t/distances[[i]]^(3/2)], 0}, planetdistances[[i]]]
     }, {i, 8}]},

   PlotRange -> All,
   ImageSize -> {450, 450},
   Boxed -> False,
   SphericalRegion -> True,
   ViewAngle -> Dynamic[zoom],
   ViewVector -> Dynamic[{camPos, targetPos}],
   ViewVertical -> {0, 0, 1}
  ]
 ],

 "X1" -> {{t, 0, "Tempo trascorso"}, 0, 50, ImageSize -> Small, AnimationRate -> 0.5,
   AnimationRepetitions -> Infinity, AnimationRunning -> False, ControlType -> Animator},
 
 (* Ripristinato esattamente come lo avevi tu *)
 "X2" -> {{zoom, .35, "Zoom"}, .5, .05, ImageSize -> Small},
 
 Delimiter,
 
 Style["Informazioni sul pianeta", Bold, 12],
 "B1" -> {{selectedPlanet, 0, "Focus su"},
   {0 -> "Sole", 1 -> "Mercurio", 2 -> "Venere", 3 -> "Terra", 4 -> "Marte",
    5 -> "Giove", 6 -> "Saturno", 7 -> "Urano", 8 -> "Nettuno"},
   ControlType -> PopupMenu},

 Dynamic@Panel[
   If[selectedPlanet == 0, 
    Style["La stella al centro del sistema solare", "Text", 11],
    Style[planetInfo[[selectedPlanet]], "Text", 11]
   ],
   ImageSize -> {200, 150},
   Alignment -> {Center, Top}
 ],
 
 Delimiter,
 
 "B2" -> {{trails, True, "Mostra orbite"}, {True, False}},
 ControlPlacement -> Left
]

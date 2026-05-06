(* ::Package:: *)

distances = {.29, .38, .56, .85, 1.3, 1.8, 2.2, 2.8};
planetdistances = {.0165, .025, .041, .029, .075, .043, .033, .026};
planetNames = {"Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune"};
planetColors = {LightGray, Orange, Blue, Red, Brown, LightOrange, LightBlue, Darker[Cyan, 0.5]};

planetInfo = {
  "Diameter: 4,879 km\nDistance from Sun: 57.9M km\nOrbital period: 88 days\nTemperature: -180\[Degree]C to 430\[Degree]C\nMoons: 0",
  "Diameter: 12,104 km\nDistance from Sun: 108.2M km\nOrbital period: 225 days\nTemperature: 462\[Degree]C\nMoons: 0",
  "Diameter: 12,742 km\nDistance from Sun: 149.6M km\nOrbital period: 365 days\nTemperature: -89\[Degree]C to 57\[Degree]C\nMoons: 1",
  "Diameter: 6,779 km\nDistance from Sun: 227.9M km\nOrbital period: 687 days\nTemperature: -140\[Degree]C to 20\[Degree]C\nMoons: 2",
  "Diameter: 139,820 km\nDistance from Sun: 778.6M km\nOrbital period: 12 years\nTemperature: -108\[Degree]C\nMoons: 95",
  "Diameter: 116,460 km\nDistance from Sun: 1.434B km\nOrbital period: 29 years\nTemperature: -139\[Degree]C\nMoons: 146",
  "Diameter: 50,724 km\nDistance from Sun: 2.871B km\nOrbital period: 84 years\nTemperature: -197\[Degree]C\nMoons: 27",
  "Diameter: 49,244 km\nDistance from Sun: 4.495B km\nOrbital period: 165 years\nTemperature: -201\[Degree]C\nMoons: 16"
};

Manipulate[
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
    Boxed -> True,
    SphericalRegion -> True,
    ViewAngle -> Dynamic[zoom]
  ],
  
  "X1" -> {{t, 0, "Time"}, 0, 50, ImageSize -> Small, AnimationRate -> 0.5, 
           AnimationRepetitions -> Infinity, AnimationRunning -> False, ControlType -> Animator},
  "X2" -> {{zoom, .35, "Zoom"}, .5, .05, ImageSize -> Small},
  Delimiter,
  Style["Planet Information", Bold, 12],
  "B1" -> {{selectedPlanet, 3, "Select Planet"}, 
           {1 -> "Mercury", 2 -> "Venus", 3 -> "Earth", 4 -> "Mars", 
            5 -> "Jupiter", 6 -> "Saturn", 7 -> "Uranus", 8 -> "Neptune"},
           ControlType -> PopupMenu},
  (* Pannello con solo le informazioni testuali *)
  Dynamic@Panel[
    Style[planetInfo[[selectedPlanet]], "Text", 11],
    ImageSize -> {200, 180},
    Alignment -> {Center, Top}
  ],
  Delimiter,
  "B2" -> {{trails, True, "Show orbits"}, {True, False}},
  ControlPlacement -> Left
]

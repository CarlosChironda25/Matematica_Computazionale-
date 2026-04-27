(* ::Package:: *)

Manipulate[distances={.29,.38,.56,.85,1.3,1.8,2.2,2.8};
	planetdistances={.0165,.025,.041,.029,.075,.043,.033,.026};
	Graphics3D[
		{Yellow,{Sphere[{0,0,0},.16]}, (*Crea il sole al centro*)
		{FaceForm[],Sphere[{0,0,0},Last[distances]]},Gray,
			If[trails,
				Table[{Opacity[0.125],
					Line[
						Table[distances[[i]]*{Cos[\[Theta]],Sin[\[Theta]],0},{\[Theta],0,2 Pi,2 Pi/100.}] (*Per disegnare le linee delle orbite*)
					]},
					{i,8}
				],{}
			],
			Table[{{LightGray,Orange,Blue,Red,Brown,LightOrange,LightBlue,Darker[Cyan,0.5]}[[i]], (*Colori dei pianeti*)
				Sphere[distances[[i]]*{Cos[t/distances[[i]]^(3/2)],Sin[t/distances[[i]]^(3/2)],0},planetdistances[[i]]]},   (*Per creare i pianeti nelle posizioni giuste e spostarli con il tempo t lungo le orbite circolari*)
				{i,8}
			]},
			PlotRange->All,
			ImageSize->{450,450}, (*Per cambiare dimensioni viewport*)
			Boxed->False,
			SphericalRegion->True,
			ViewAngle->Dynamic[zoom]
	],
	"X1"->{{t,0,"Time"},0,50,ImageSize->Small,AnimationRate->0.5,AnimationRepetitions->Infinity,AnimationRunning->False,ControlType->Animator},  (*Slider per il tempo*)
	"X2"->{{zoom,.35,"Zoom"},.5,.05,ImageSize->Small},  (*Slider per lo zoom*)
	"B2"->{{trails,True,"Show orbits"},{True,False}},  (*Checkbox per mostrare le orbite, attivo di default*)
	ControlPlacement->Left
]

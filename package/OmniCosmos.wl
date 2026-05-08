BeginPackage["OmniCosmos`"]

LaunchOmniCosmos::usage = "Avvia il modulo Accademia con risposta libera.";

Begin["`Private`"]



(* ===================== *)
(* caricamento database *)
(* ===================== *)

packageDir = DirectoryName[$InputFileName];

dataPath = FileNameJoin[
  {ParentDirectory[packageDir], "Data", "Database.wl"}
];

exerciseDB = Import[dataPath, "WL"];

If[exerciseDB === $Failed || !ListQ[exerciseDB],
  exerciseDB = {};
];

normalizeText[str_] := Module[{s = str},
  s = ToLowerCase@StringTrim[str];
  s = StringReplace[s, {
    "à"->"a","á"->"a","â"->"a",
    "è"->"e","é"->"e","ê"->"e",
    "ì"->"i","í"->"i","î"->"i",
    "ò"->"o","ó"->"o","ô"->"o",
    "ù"->"u","ú"->"u","û"->"u"
  }];
  s
];

exerciseDB = Map[
  If[AssociationQ[#] && KeyExistsQ[#, "answer"] && StringQ[#["answer"]] && #["type"] === "text",
    Append[#, "answer" -> normalizeText[#["answer"]]],
    #
  ] &,
  exerciseDB
];

Echo[Length[exerciseDB], "Esercizi caricati:"];

(* ===================== *)
(* PHYSICS GENERATOR *)
(* ===================== *)
G = 6.674*10^-11;
generatePhysicsExercise[] := Module[{m1, m2, r},

  m1 = RandomReal[{10^23, 10^25}];
  m2 = RandomReal[{10^29, 10^31}];
  r  = RandomReal[{10^10, 10^12}];

  <|
    "level" -> 3,
    "type" -> "numeric",
    "question" -> "Calcola la forza gravitazionale (Newton)",
    "data" -> <|"m1"->m1,"m2"->m2,"r"->r|>,
    "answer" -> G*m1*m2/r^2,
    "hint" -> "F = G*m1*m2/r^2"
  |>
];

(* ===================== *)
(* GET EXERCISE *)
(* ===================== *)

getExercise[level_] := Module[{filtered},

  If[level === 3,
    Return[generatePhysicsExercise[]]
  ];

  filtered = Select[exerciseDB, #["level"] == level &];

  If[filtered === {} || filtered === $Failed,
    Return[<|
      "level" -> level,
      "type" -> "text",
      "question" -> "Nessun esercizio disponibile",
      "answer" -> "",
      "hint" -> ""
    |>]
  ];

  RandomChoice[filtered]
];

(* ===================== *)
(* CHECK ANSWER *)
(* ===================== *)

checkAnswer[exercise_, user_] := Module[
  {numericUser, diff, ans},

  Which[

    exercise["type"] === "text",
    normalizeText[user] === exercise["answer"],

    exercise["type"] === "numeric",

    numericUser = Quiet@Check[Interpreter["Number"][user], $Failed];
    If[numericUser === $Failed, Return["wrong"]];

    ans = exercise["answer"];
    diff = Abs[numericUser - ans]/Max[Abs[ans], 10^-30];

    Which[
      diff < 10^-3, "perfect",
      diff < 10^-1, "close",
      True, "wrong"
    ],

    True,
    "wrong"
  ]
];

(* ===================== *)
(* HINT SYSTEM (CYCLIC) *)
(* ===================== *)

getHints[ex_] := Module[{},
  Which[
    KeyExistsQ[ex, "hints"] && ListQ[ex["hints"]],
    ex["hints"],

    KeyExistsQ[ex, "hint"] && StringQ[ex["hint"]],
    {ex["hint"]},

    True,
    {}
  ]
];

(* ===================== *)
(* LAUNCH UI *)
(* ===================== *)

LaunchOmniCosmos[] := DynamicModule[
 {
  userAnswer = "",
  feedback = "",
  level = 1,
  score = 0,
  currentExercise = getExercise[1],
  alreadyAnswered = False,
  showAnswer = False,

  hintIndex = 0
 },

 Panel[
  Column[{

    Style["🌌 Esercitazioni", 20, Bold],

    Row[{"Score: ", Dynamic[score]}],

    SetterBar[
     Dynamic[level,
      (level = #;
       currentExercise = getExercise[level];
       userAnswer = "";
       feedback = "";
       alreadyAnswered = False;
       showAnswer = False;
       hintIndex = 0;
      ) &
     ],
     {1->"Facile",3->"Difficile"}
    ],

    Button["Nuova domanda",
     currentExercise = getExercise[level];
     userAnswer = "";
     feedback = "";
     alreadyAnswered = False;
     showAnswer = False;
     hintIndex = 0;
    ],

    Dynamic[
     Column[{Style[currentExercise["question"],14]}]
    ],

    InputField[
      Dynamic[userAnswer],
      String,
      Enabled -> (!showAnswer && !alreadyAnswered)
    ],

    (* ===================== *)
    (* HINT BUTTON CYCLIC *)
    (* ===================== *)

    Button["💡 Aiuto",
      Module[{hints = getHints[currentExercise], n},
        n = Length[hints];
        If[n > 0,
          hintIndex = Mod[hintIndex, n] + 1;
          feedback = hints[[hintIndex]]
        ]
      ]
    ],

    (* ===================== *)
    (* VERIFY *)
    (* ===================== *)

    Button["Verifica",
     Module[{result = checkAnswer[currentExercise, userAnswer]},

       If[(result === True || result === "perfect") && !alreadyAnswered,
        score++;
        alreadyAnswered = True;
       ];

       feedback = Which[
         result === True || result === "perfect",
         "✅ Corretto!",

         result === "close",
         "⚠️ Vicino!",

         True,
         "❌ Errato"
       ];
     ]
    ],

    Button["Risposta", showAnswer = True;],

    Dynamic[Style[feedback,14]],

    Dynamic[
     If[showAnswer,
      Style[
        "Risposta: " <>
        If[currentExercise["type"]==="numeric",
          ToString@NumberForm[currentExercise["answer"],4],
          currentExercise["answer"]
        ],
        14, Blue
      ],
      ""
     ]
    ]

  }]
 ]
]

End[]

EndPackage[]
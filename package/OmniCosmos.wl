BeginPackage["OmniCosmos`"]

LaunchOmniCosmos::usage = "Avvia il modulo Accademia con risposta libera.";

Begin["`Private`"]

(* ===================== *)
(* COSTANTE *)
(* ===================== *)

G = 6.674*10^-11;

(* ===================== *)
(* DATABASE *)
(* ===================== *)

packageDir = DirectoryName[$InputFileName];

dataPath = FileNameJoin[
  {ParentDirectory[packageDir], "Data", "Database.wl"}
];

exerciseDB = Import[dataPath, "WL"];

(* sicurezza import *)
If[exerciseDB === $Failed || !ListQ[exerciseDB],
  exerciseDB = {};
];

Echo[Length[exerciseDB], "Esercizi caricati:"];

(* ===================== *)
(* GENERATORE FISICA *)
(* ===================== *)

generatePhysicsExercise[] := Module[{m1, m2, r},

  m1 = RandomReal[{10^23, 10^25}];
  m2 = RandomReal[{10^29, 10^31}];
  r  = RandomReal[{10^10, 10^12}];

  <|
    "level" -> 3,
    "type" -> "numeric",
    "question" -> "Calcola la forza gravitazionale (Newton)",
    "data" -> <|"m1" -> m1, "m2" -> m2, "r" -> r|>,
    "answer" -> G*m1*m2/r^2,
    "hint" -> "F = G m1 m2 / r^2"
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
      "hint" -> "Controlla il database"
    |>]
  ];

  RandomChoice[filtered]
];

(* ===================== *)
(* VALIDAZIONE ROBUSTA *)
(* ===================== *)

checkAnswer[exercise_, user_] := Module[
  {cleanedUser, numericUser, diff, ans},

  cleanedUser = ToLowerCase@StringTrim[user];

  Which[
    
    exercise["type"] === "text",
    cleanedUser === ToLowerCase@exercise["answer"],

    exercise["type"] === "numeric",

    numericUser = Quiet@Check[Interpreter["Number"][user], $Failed];
    If[numericUser === $Failed, Return["wrong"]];

    ans = exercise["answer"];

    diff = If[
      ans == 0,
      Abs[numericUser - ans],
      Abs[numericUser - ans]/Abs[ans]
    ];

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
(* UI PRINCIPALE *)
(* ===================== *)

LaunchOmniCosmos[] := DynamicModule[
  {
    userAnswer = "",
    feedback = "",
    level = 1,
    score = 0,
    currentExercise
  },

  currentExercise := getExercise[level];

  Panel[
   Column[{

     Style["🌌 Modulo Accademia", 20, Bold],

     Row[{"Score: ", Dynamic[score]}],

     SetterBar[
      Dynamic[level],
      {1 -> "Facile", 3 -> "Difficile"}
     ],

     Button["Nuova domanda",
      userAnswer = "";
      feedback = "";
      currentExercise = getExercise[level];
     ],

     Dynamic[
      Module[{ex = currentExercise},

       Column[{

         Style[ex["question"], 14],

         If[AssociationQ[ex] && KeyExistsQ[ex, "data"],
          Grid[KeyValueMap[{#1, "=", #2} &, ex["data"]]],
          Nothing
         ]

       }]
      ]
     ],

     InputField[Dynamic[userAnswer], String],

     Button["Verifica",
      Module[{result = checkAnswer[currentExercise, userAnswer]},

       feedback = Which[
         
         result === True || result === "perfect",
         score++;
         "✅ Corretto!",

         result === "close",
         "⚠️ Vicino! Controlla le unità.",

         True,
         "❌ Sbagliato → " <> currentExercise["hint"]
       ];
      ]
     ],

     Dynamic[Style[feedback, 14]]

   }]
  ]
];

End[]

EndPackage[]
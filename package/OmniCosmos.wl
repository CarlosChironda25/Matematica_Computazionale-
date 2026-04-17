(* ::Package:: *)

BeginPackage["OmniCosmos`"]

LaunchOmniCosmos::usage = "Avvia il modulo Accademia con risposta libera.";


Begin["`Private`"]

(* ===================== *)
(* COSTANTE *)
(* ===================== *)

G = 6.674*10^-11;

(* ===================== *)
(* DATABASE ESERCIZI *)
(* ===================== *)

exerciseDB = {

  <|"level" -> 1,
    "type" -> "text",
    "question" -> "Qual è il pianeta più grande?",
    "answer" -> "giove",
    "hint" -> "È il quinto pianeta"|>,

  <|"level" -> 1,
    "type" -> "text",
    "question" -> "Qual è il pianeta più vicino al Sole?",
    "answer" -> "mercurio",
    "hint" -> "È il primo pianeta"|>

}
(* ===================== *)
(* GENERATORE DINAMICO *)
(* ===================== *)

generatePhysicsExercise[] := Module[{m1, m2, r},

  m1 = RandomReal[{10^23, 10^25}];
  m2 = RandomReal[{10^29, 10^31}];
  r = RandomReal[{10^10, 10^12}];

  <|"level" -> 3,
    "type" -> "numeric",
    "question" -> "Calcola la forza gravitazionale (Newton)",
    "data" -> {"m1" -> m1, "m2" -> m2, "r" -> r},
    "answer" -> G*m1*m2/r^2,
    "hint" -> "Usa F = G m1 m2 / r^2"|>
];

(* ===================== *)
(* FILTRAGGIO (Cases) *)
(* ===================== *)

(*scegliere la domanda rispetto al livelo*)
getExercise[level_] := Module[{filtered},

  filtered = Cases[exerciseDB, x_ /; x["level"] == level];

  If[level == 3,
    generatePhysicsExercise[],
    RandomChoice[filtered]
  ]
];
 
(* ===================== *)
(* VALIDAZIONE *)
(* ===================== *)

checkAnswer[exercise_, user_] := Module[
  {cleanedUser, numericUser, diff},

  cleanedUser = ToLowerCase@StringTrim[user];

  Which[

   (* Risposta testuale *)
   exercise["type"] == "text",
   cleanedUser === exercise["answer"],

   (* Risposta numerica *)
   exercise["type"] == "numeric" && MatchQ[ToExpression[user], _?NumericQ],

   numericUser = ToExpression[user];
   diff = Abs[numericUser - exercise["answer"]];

   Which[
    diff < 10^10, "perfect",
    diff < 10^20, "close",
    True, "wrong"
   ],

   True, False
  ]
];

(* ===================== *)
(* INTERFACCIA *)
(* ===================== *)

LaunchOmniCosmos[] := DynamicModule[
  {exercise, userAnswer = "", feedback = "", level = 1, score = 0},

  exercise = getExercise[level];

  Panel[
   Column[{

     Style["🌌 Modulo Accademia", 20, Bold],

     Row[{"Score: ", Dynamic[score]}],

     SetterBar[Dynamic[level], {
       1 -> "Facile",
       3 -> "Difficile"
     }],

     Button["Nuova domanda",
      exercise = getExercise[level];
      userAnswer = "";
      feedback = "";
     ],

     Dynamic[
      Column[{

        Style[exercise["question"], 14],

        (* Visualizzazione dati con ReplaceAll + Rule + Flatten *)
        If[KeyExistsQ[exercise, "data"],
         Grid[
          Partition[
           Flatten[
            exercise["data"] /. Rule[a_, b_] :> {a, "=", b}
           ], 3
          ]
         ],
         Nothing
        ]

      }]
     ],

     (* Input libero *)
     InputField[Dynamic[userAnswer], String],

     Button["Verifica",
      Module[{result = checkAnswer[exercise, userAnswer]},

       feedback = Which[

         result === True,
         score++;
         "✅ Corretto!",

         result === "perfect",
         score++;
         "✅ Perfetto!",

         result === "close",
         "⚠️ Vicino! Controlla le unità.",

         True,
         "❌ Sbagliato → " <> exercise["hint"]
       ];
      ]
     ],

     Dynamic[Style[feedback, 14]]

   }]
  ]
];

End[]

EndPackage[]
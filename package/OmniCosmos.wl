(* ============================== *)
(*      OmniCosmos Package        *)
(* ============================== *)

BeginPackage["OmniCosmos`"]

LaunchOmniCosmos::usage =
 "LaunchOmniCosmos[] avvia l'interfaccia interattiva per esercitazioni.";

Begin["`Private`"]

(* ============================== *)
(* DATABASE LOADING              *)
(* ============================== *)

packageDir = DirectoryName[$InputFileName]; (* directory del file .m *)

dataPath = FileNameJoin[{ParentDirectory[packageDir], "Data", "Database.wl"}]; (* path al database *)

exerciseDB = Import[dataPath, "WL"]; (* import esercizi *)

If[exerciseDB === $Failed || !ListQ[exerciseDB],
  exerciseDB = {}; (* fallback sicuro se import fallisce *)
];

(* ============================== *)
(* TEXT NORMALIZATION            *)
(* ============================== *)

normalizeText[str_] := Module[{s},

  s = ToLowerCase@StringTrim[str]; (* rimuove spazi + lowercase *)

  s = StringReplace[s, {        (* rimozione accenti *)
    "à" -> "a","á" -> "a","â" -> "a",
    "è" -> "e","é" -> "e","ê" -> "e",
    "ì" -> "i","í" -> "i","î" -> "i",
    "ò" -> "o","ó" -> "o","ô" -> "o",
    "ù" -> "u","ú" -> "u","û" -> "u"
  }];

  s
];

(* ============================== *)
(* PREPROCESS DATABASE           *)
(* ============================== *)

exerciseDB = Map[
  If[
    AssociationQ[#] && KeyExistsQ[#, "answer"] && StringQ[#["answer"]],

    Append[#, "answer" -> normalizeText[#["answer"]]], (* normalizza risposta *)

    #
  ] &,
  exerciseDB
];

Echo[Length[exerciseDB], "Esercizi caricati:"]; (* debug numero esercizi *)

(* ============================== *)
(* EXERCISE SELECTION           *)
(* ============================== *)

getExercise[level_] := Module[{filtered},

  filtered = Select[exerciseDB, #["level"] == level &]; (* filtro per livello *)

  If[filtered === {},
    Return[
      <|
        "level" -> level,
        "question" -> "Nessun esercizio disponibile",
        "answer" -> "",
        "hint" -> ""
      |>
    ]
  ];

  RandomChoice[filtered] (* selezione casuale *)
];

(* ============================== *)
(* ANSWER CHECK                 *)
(* ============================== *)

checkAnswer[exercise_, user_] :=
  normalizeText[user] === exercise["answer"]; (* confronto normalizzato *)

(* ============================== *)
(* HINT SYSTEM                 *)
(* ============================== *)

getHints[ex_] := Module[{},
  Which[
    KeyExistsQ[ex, "hints"] && ListQ[ex["hints"]],
    ex["hints"], (* lista di suggerimenti *)

    KeyExistsQ[ex, "hint"] && StringQ[ex["hint"]],
    {ex["hint"]}, (* singolo hint convertito in lista *)

    True,
    {}
  ]
];

(* ============================== *)
(* MAIN UI                     *)
(* ============================== *)

LaunchOmniCosmos[] := DynamicModule[

  {

    userAnswer = "",          (* input utente *)
    feedback = "",            (* messaggi feedback *)
    level = 1,                (* livello selezionato *)

    correctAnswers = 0,       (* risposte corrette *)
    totalQuestions = 0,       (* domande totali *)

    currentExercise = getExercise[1], (* esercizio corrente *)

    showAnswer = False,       (* toggle risposta *)
    hintIndex = 0,            (* indice hint *)

    counted = False           (* evita doppio conteggio *)
  },

  Panel[
    Column[{

      Style["🌌 Esercitazioni", 20, Bold],

      Row[{
        "Score: ",
        Dynamic[correctAnswers], (* aggiornamento dinamico *)
        " / ",
        Dynamic[totalQuestions]
      }],

      Row[{

        SetterBar[
          Dynamic[
            level,
            (
              level = #;
              currentExercise = getExercise[level]; (* aggiorna esercizio *)
              userAnswer = "";
              feedback = "";
              showAnswer = False;
              hintIndex = 0;
              counted = False;
            ) &
          ],
          {
            1 -> "Facile"
          }
        ],

        Button["Pulire",
          correctAnswers = 0;
          totalQuestions = 0;
          userAnswer = "";
          feedback = "";
          showAnswer = False;
          hintIndex = 0;
          counted = False;
        ]

      }],

      Button["Nuova domanda",
        currentExercise = getExercise[level];
        userAnswer = "";
        feedback = "";
        showAnswer = False;
        hintIndex = 0;
        counted = False;
      ],

      Dynamic[
        Style[currentExercise["question"], 14]
      ],

      InputField[
        Dynamic[userAnswer],
        String,
        Enabled -> !showAnswer
      ],

      Button["💡 Aiuto",
        Module[{hints = getHints[currentExercise], n},

          n = Length[hints]; (* numero hints disponibili *)

          If[n > 0,
            hintIndex = Mod[hintIndex, n] + 1;
            feedback = hints[[hintIndex]];
          ]
        ]
      ],

      Button["Verifica",
        Module[{result},

          result = checkAnswer[currentExercise, userAnswer]; (* controllo risposta *)

          feedback =
            If[result,
              "✅ Corretto!",
              "❌ Errato"
            ];

          If[!counted,
            totalQuestions++; (* incremento totale *)
            If[result, correctAnswers++;]; (* incremento corrette *)
            counted = True;
          ];

        ]
      ],

      Button["Risposta",
        showAnswer = True;
      ],

      Dynamic[Style[feedback, 14]],

      Dynamic[
        If[showAnswer,
          Style[
            "Risposta: " <> currentExercise["answer"],
            14,
            Blue
          ],
          ""
        ]
      ]

    }]
  ]

]

End[]

EndPackage[]
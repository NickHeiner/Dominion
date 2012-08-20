module Bot

open Definitions

let standard buys = 
    let provincesAlmostGone = CardsRemainingLessThan (5, Victory Province)
    [(Always, Victory Province)] @ buys @ [(provincesAlmostGone, Victory Province);
                                           (provincesAlmostGone, Victory Duchy);
                                           (provincesAlmostGone, Victory Estate)]

let smithy = "Smithy", [(Always, ASmithy)], standard [(ExpectedPerHandLessThan (1., Action Smithy), Action Smithy);
                                                      (Always, Coin Gold);
                                                      (Always, Coin Silver)]

let mineSmithy = "MineSmithy", 
                 [(Always, AMine Silver);
                  (Always, AMine Copper);
                  (Always, ASmithy)
                  (Always, AMine Gold)],

                 standard [(ExpectedPerHandLessThan (0.5, Action Smithy), Action Smithy);
                           (ExpectedPerHandLessThan (0.5, Action Mine), Action Mine);
                           (Always, Coin Gold);
                           (Always, Coin Silver)]

let dupee (name, acts, buys) = name + "dupee", acts, buys 

(* If these names aren't valid for Excel sheets, or you have duplicate bot names, you're gonna have a bad time. *)
let bots = ["Pass", [], []; smithy; mineSmithy; dupee smithy]
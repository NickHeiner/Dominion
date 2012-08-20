module Bot

open Definitions

let standard buys = 
    let provincesAlmostGone = CardsRemainingLessThan (5, Victory Province)
    [(Always, Victory Province)] @ buys @ [(provincesAlmostGone, Victory Province);
                                           (provincesAlmostGone, Victory Duchy);
                                           (provincesAlmostGone, Victory Estate)]

let smithy = [(Always, ASmithy)], standard [(ExpectedPerHandLessThan (1., Action Smithy), Action Smithy);
                                            (Always, Coin Gold);
                                            (Always, Coin Silver)]

let mineSmithy = [(Always, AMine Silver);
                  (Always, AMine Copper);
                  (Always, ASmithy)
                  (Always, AMine Gold)],

                 standard [(ExpectedPerHandLessThan (0.5, Action Smithy), Action Smithy);
                           (ExpectedPerHandLessThan (0.5, Action Mine), Action Mine);
                           (Always, Coin Gold);
                           (Always, Coin Silver)]

(* If these names aren't valid for Excel sheets, you're gonna have a bad time. *)
let bots = ["Pass", ([], []); "Smithy", smithy; "MineSmithy", mineSmithy]
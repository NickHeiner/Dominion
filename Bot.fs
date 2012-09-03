module Bot

open Definitions

let standard buys = 
    (* TODO this could be more sophisticated.
       Really what we want is "game almost over" which is a superset of "provinces almost gone" *)
    let provincesAlmostGone = CardsRemainingLessThan (5, Victory Province)
    [(Always, Victory Province)
     (provincesAlmostGone, Victory Province)
     (provincesAlmostGone, Victory Duchy)
     (provincesAlmostGone, Victory Estate)] @ buys

let maintainExpectedPerHand expected card = ExpectedPerHandLessThan (expected, card), card
let buyAtMost count card = CountInCardsLessThan (count, card), card
let cellarVictories = Always, ACellar <| CellarChoice (List.filter (function Victory _ -> true | _ -> false))
let maintainEqualCounts card card' = [MoreOfFirst (card, card'), card'
                                      MoreOfFirst (card', card), card]

let smithy = "Smithy", [(Always, ASmithy)], standard [maintainExpectedPerHand 1. <| Action Smithy
                                                      (Always, Coin Gold)
                                                      (Always, Coin Silver)]

let mineSmithy = "MineSmithy", 
                 [Always, AMine Silver
                  Always, AMine Copper
                  Always, ASmithy
                  Always, AMine Gold],

                 standard [maintainExpectedPerHand 0.5 <| Action Smithy
                           maintainExpectedPerHand 0.5 <| Action Mine
                           (Always, Coin Gold)
                           (Always, Coin Silver)]

let mineSmithy2 = 
    "MineSmithy2", 
    [Always, AMine Silver
     Always, AMine Copper
     Always, ASmithy
     Always, AMine Gold],

    standard ((maintainEqualCounts (Action Smithy) (Action Mine)) @ [(Always, Coin Gold); (Always, Coin Silver)])



let hugeBitchCellar = "HUGE BITCH Cellar",
                      [Always, AVillage
                       cellarVictories
                       Always, AThroneRoom AWitch
                       Always, AWitch],

                      standard [Always, Coin Gold
                                maintainExpectedPerHand 1. <| Action Witch
                                maintainExpectedPerHand 1. <| Action ThroneRoom
                                Always, Coin Silver; (* but sometimes we want a village, too *)
                                CountInCardsLessThan (1, Action Cellar), Action Cellar]


let hugeBitch = "HUGE BITCH",
                [Always, AVillage
                 Always, AThroneRoom AWitch
                 Always, AWitch],

                standard [Always, Coin Gold;
                          maintainExpectedPerHand 1. <| Action Witch
                          maintainExpectedPerHand 1. <| Action ThroneRoom
                          Always, Coin Silver; (* but sometimes we want a village, too *)
                         ]

let cash = "Ca$hMonay",
           [],
           standard [Always, Coin Gold
                     Always, Coin Silver]
                     
let gardens = "Gardens up in this bitch",
              [Always, AVillage
               cellarVictories
               Always, AFestival
               Always, AMarket
               Always, ACouncilRoom],

              standard [Always, Coin Gold;

                        (* 5 treasure *)
                        Always, Action Market
                        Always, Action Festival
                        Always, Action CouncilRoom
                        Always, Victory Duchy

                        (* 4 treasure *)
                        Always, Victory Gardens

                        (* 3 treasure *) 
                        Always, Coin Silver
                        Always, Action Village

                        (* 2 treasure *)
                        Always, Action Cellar
                        Always, Victory Estate
              ]
               
let remodel = "Remodel",
              [Always, AVillage
               cellarVictories 
               Always, AFestival
               Always, AMarket
               (* What else to remodel? *)
               Always, ARemodel (Coin Gold, Victory Province)],

              standard [Always, Coin Gold
                        buyAtMost 2 <| Action Mine
                        maintainExpectedPerHand 1. <| Action Remodel
                        Always, Action ThroneRoom
                        Always, Action Village
                        Always, Coin Silver
                        Always, Victory Estate]

let chapel = "Chapel",
             [Always, AVillage
              Always, AFestival
              Always, ALaboratory
              Always, AMine Silver
              Always, AMine Copper
              Always, ARemodel (Coin Gold, Victory Province)
              Always, AChapel (Some <| Coin Copper, Some <| Victory Estate, None, None)
              Always, AMoneylender
              Always, AMine Gold],

             standard [Always, Coin Gold
                       (* >= 4 treasure *)
                       buyAtMost 1 <| Action Mine
                       buyAtMost 1 <| Action CouncilRoom
                       buyAtMost 1 <| Action Festival

                       (* < 4 treasure *)
                       buyAtMost 1 <| Action Chapel
                       Always, Action Laboratory
                       buyAtMost 1 <| Action Moneylender
                       Always, Coin Silver
                       Always, Action Village
                       buyAtMost 1 <| Action Cellar]

(* If these names aren't valid for Excel sheets, or you have duplicate bot names, you're gonna have a bad time. *)
let bots = ["Pass", [], []
            smithy
            mineSmithy
            cash
            mineSmithy2
            gardens]
module Definitions

type VictCard = Province | Duchy | Estate | Gardens | Curse
type CoinCard = Gold | Silver | Copper 
type ActCard = Cellar | Chapel | Chancellor | Village | Woodcutter | Feast | Militia | Moneylender | Remodel | Smithy 
                | Spy | Thief | ThroneRoom | CouncilRoom | Festival | Laboratory | Library | Market | Mine | Witch | Adventurer

type card = Victory of VictCard | Coin of CoinCard | Action of ActCard

type player = {hand : card list; deck : card list; discard : card list}

(* this there a better way to enumerate over all members of the type? *)
let allCards = [Victory Province; Victory Duchy; Coin Copper; Action Smithy]

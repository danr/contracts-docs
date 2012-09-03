module Head where

import Contracts
import Prelude

contract_1 = head ::: CF --> CF

contract_2 = head ::: Pred (not . null) --> CF

contract_3 = head ::: CF :&: Pred (not . null) --> CF

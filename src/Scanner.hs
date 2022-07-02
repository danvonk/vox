-- |

module Scanner where

import Tokens (TokenType)

data Scanner = Scanner
  {
    source :: String,
    tokens :: [TokenType]
  }


scanTokens :: Scanner -> Scanner
scanTokens s = undefined

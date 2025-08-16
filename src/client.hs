
import Miso (miso, run)

import App.Component (appComponent)

foreign export javascript "hs_start" main :: IO ()

main :: IO ()
main = run (miso $ const appComponent)


import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (withHSlides)
import Prelude              (IO)

main :: IO ()
main = defaultMain fromArgs withHSlides
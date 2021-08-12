-- | Settings used by the code generator to customize the code.
module Copilot.Compile.C99.Settings
  {-# DEPRECATED "This module will be hidden in future versions." #-}
  where

-- | Settings used to customize the code generated.
data CSettings = CSettings
  { cSettingsStepFunctionName :: String
  }

-- | Default settings with a step function called @step@.
mkDefaultCSettings :: CSettings
mkDefaultCSettings = CSettings "step"

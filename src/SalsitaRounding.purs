module SalsitaRounding where

data Tag = Devops | Meeting | Research | Review | Other

data TogglEntry = TogglEntry String String Tag Int

data TimelyEntry = TimelyEntry String String Tag Int

roundToQuarters :: Array TogglEntry -> Array TimelyEntry
roundToQuarters _ = []

module Log.Types where

data FileLogSpec = FileLogSpec {
    log_file :: String
  , log_file_size :: Int
  , log_backup_number :: Int
  }

module ScrapeEngine.CsvOut where

import qualified Data.Csv             as Csv
import           Pipes                (Consumer, liftIO, runEffect, yield, (>->))
import qualified Pipes.ByteString     as PB
import           Pipes.Csv            (encode)
import           Pipes.Csv.Encoding   (namedRecordToRecord)
import           Pipes.Safe           (MonadSafe, runSafeT)
import qualified Pipes.Safe           as PS
import           ScrapeEngine.Prelude
import qualified System.IO            as Io

write :: MonadSafe m => Path b File -> Consumer ByteString m ()
write f = withFile f Io.WriteMode Io.LineBuffering PB.toHandle

withFile :: (MonadSafe m) => Path b File -> Io.IOMode -> Io.BufferMode -> (Io.Handle -> m r) -> m r
withFile file ioMode bufferMode = PS.bracket
  (liftIO $ do
    h <- Io.openFile (toFilePath file) ioMode
    Io.hSetBuffering h bufferMode
    pure h)
  (liftIO . Io.hClose)

writeCsv :: forall r b. (Show r, Csv.DefaultOrdered r, Csv.ToNamedRecord r)
          => IO r -> Path b File -> IO ()
writeCsv readChan file = runSafeT $ runEffect pipeline
  where
    header = Csv.headerOrder (undefined :: r)
    getRecord = do
      x <- liftIO readChan
      yield $ namedRecordToRecord header $ Csv.toNamedRecord x

    pipeline = (yield (Csv.toRecord header) >> forever getRecord) >-> encode >-> write file

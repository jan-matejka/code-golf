import Jmcgmqp (test_prometheus)
import Jmcgmqp.Prometheus (cmdTestPrometheus)
import Jmcgmqp.Runtime (newInstance, config)
import Jmcgmqp.Worker (cmdRun)

main :: IO ()
main = newInstance >>= dispatch . test_prometheus . config

dispatch :: Int -> IO ()
dispatch 1 = cmdTestPrometheus
dispatch _ = cmdRun

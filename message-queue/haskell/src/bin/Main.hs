import Jmcgmqp (test_prometheus)
import Jmcgmqp.Prometheus (cmdTestPrometheus)
import Jmcgmqp.Runtime (newInstance, config, Instance)
import Jmcgmqp.Algorithm (cmdRun)

main :: IO ()
main = newInstance >>= \x -> dispatch (test_prometheus $ config x) x

dispatch :: Int -> Instance -> IO ()
dispatch 1 = cmdTestPrometheus
dispatch _ = cmdRun

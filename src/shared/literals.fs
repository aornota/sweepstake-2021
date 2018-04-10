module Aornota.Sweepstake2018.Shared.Literals

let [<Literal>] WS_PORT = 
#if DEBUG
    8084us
#else
    8088us
#endif
let [<Literal>] WS_API_PATH = "/api/ws"

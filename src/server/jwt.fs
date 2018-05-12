module Aornota.Sweepstake2018.Server.Jwt

open Aornota.Common.Json

open Aornota.Server.Common.JsonConverter

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Server.Authorization

open System.IO
open System.Security.Cryptography

open Jose

let [<Literal>] private JWT_KEY_FILE = "./secret/jwt.txt"

let private jwtKey =
    let file = FileInfo JWT_KEY_FILE
    if file.Exists |> not then
        if file.Directory.Exists |> not then file.Directory.Create ()
        let bytes : byte [] = Array.zeroCreate 32
        (RandomNumberGenerator.Create ()).GetBytes bytes
        File.WriteAllBytes (file.FullName, bytes)
    File.ReadAllBytes file.FullName

let private encode (Json json) = JWT.Encode (json, jwtKey, JweAlgorithm.A256KW, JweEncryption.A256CBC_HS512)
let private decode text = JWT.Decode (text, jwtKey, JweAlgorithm.A256KW, JweEncryption.A256CBC_HS512) |> Json

let private authUser userId userName userType permissions jwt = { UserId = userId ; UserName = userName ; UserType = userType ; Permissions = permissions ; Jwt = jwt }

let toAuthUser (sessionId:SessionId, userId, userName, userType, permissions, userTokens:UserTokens) =
    try
        let jwt = (sessionId, userId, userName, userType, permissions, userTokens) |> toJson |> encode |> Jwt
        authUser userId userName userType permissions jwt |> Ok
    with | exn -> exn.Message |> Error

let fromJwt (Jwt jwt) =
    try
        let sessionId, userId, userName, userType, permissions, userTokens = jwt |> decode |> ofJson<SessionId * UserId * UserName * UserType * Permissions * UserTokens>
        (sessionId, authUser userId userName userType permissions (Jwt jwt), userTokens) |> Ok
    with | exn -> exn.Message |> Error

module Aornota.Sweepstake2018.Server.Jwt

//open Aornota.Server.Common.JsonConverter

//open Aornota.Sweepstake2018.Common.Domain.Core

(* TEMP-NMB: Copied from fable-suave-scaffold [WIP]: JsonWebToken.fs...
open System
open System.IO
open System.Security.Cryptography
open System.Text

open ServerTypes

open Newtonsoft.Json

let private createPassPhrase() = let crypto, randomNumber = RandomNumberGenerator.Create(), byte |> Array.init 32
                                 randomNumber |> crypto.GetBytes
                                 randomNumber

let private passPhrase = let encoding = Encoding.UTF8
                         let fi = "./temp/token.txt" |> FileInfo
                         if not fi.Exists then let passPhrase = createPassPhrase()
                                               if not fi.Directory.Exists then
                                                   fi.Directory.Create()
                                               File.WriteAllBytes(fi.FullName, passPhrase)
                         fi.FullName |> File.ReadAllBytes

let private encodeString (payload:string) = Jose.JWT.Encode(payload, passPhrase, Jose.JweAlgorithm.A256KW, Jose.JweEncryption.A256CBC_HS512)

let private decodeString (jwt:string) = Jose.JWT.Decode(jwt, passPhrase, Jose.JweAlgorithm.A256KW, Jose.JweEncryption.A256CBC_HS512)

let encode token = token |> JsonConvert.SerializeObject |> encodeString

let decode<'a> (jwt:string) : 'a = jwt |> decodeString |> JsonConvert.DeserializeObject<'a>

let isValid (jwt:string) : UserRights option = try let token = jwt |> decode
                                                   Some token
                                               with | _ -> None*)

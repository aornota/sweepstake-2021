module Aornota.Sweepstake2018.Common.Domain.UserAdmin

open Aornota.Common.Revision

open Aornota.Sweepstake2018.Common.Domain.User

open System

type User4AdminDto = { UserId : UserId ; Rvn : Rvn ; UserName : UserName ; UserType : UserType ; LastActivity : DateTimeOffset option }

type UserAdminProjectionDto = { User4AdminDtos : User4AdminDto list }

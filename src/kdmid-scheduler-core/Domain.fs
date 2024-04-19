module KdmidScheduler.Domain

module Core =
    type UserId = Id of string
    type KdmidCredentialId = Id of string
    type KdmidCredentialCd = Cd of string
    type KdmidCredentialEms = Ems of string option

    type User = { Id: UserId; Name: string }

    type KdmidCredentials =
        { Id: KdmidCredentialId
          Cd: KdmidCredentialCd
          Ems: KdmidCredentialEms }

    type City =
        | Belgrade
        | Budapest
        | Sarajevo

    type UserKdmidOrders =
        { User: User
          Orders: Map<City, Set<KdmidCredentials>> }

module Persistence =

    type User = { Id: string; Name: string }

    type KdmidCredentials = { Id: string; Cd: string; Ems: string }

    type UserKdmidOrders =
        { User: User
          Orders: Map<string, Set<KdmidCredentials>> }

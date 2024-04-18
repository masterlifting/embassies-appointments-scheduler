module KdmidScheduler.Domain

module Core =

    type City =
        | Belgrade of string
        | Budapest of string
        | Sarajevo of string

    type KdmidCredentialId = Id of string
    type KdmidCredentialCd = Cd of string
    type KdmidCredentialEms = Ems of string option

    type KdmidCredentials =
        { Id: KdmidCredentialId
          Cd: KdmidCredentialCd
          Ems: KdmidCredentialEms }

    type KdmidQueueItem =
        { City: City
          KdmidCredentials: KdmidCredentials }

module Persistence =

    type KdmidCredentials = { Id: string; Cd: string; Ems: string }

    type KdmidQueueItem =
        { City: string
          KdmidCredentials: KdmidCredentials }

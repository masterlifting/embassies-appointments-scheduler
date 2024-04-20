module KdmidScheduler.Domain

module Core =
    type City =
        | Belgrade
        | Budapest
        | Sarajevo

    type UserId = UserId of string
    type User = { Id: UserId; Name: string }

    type KdmidCredentialId = KdmidCredentialId of string
    type KdmidCredentialCd = KdmidCredentialCd of string
    type KdmidCredentialEms = KdmidCredentialEms of string option

    type KdmidCredentials =
        { Id: KdmidCredentialId
          Cd: KdmidCredentialCd
          Ems: KdmidCredentialEms }

    type UserCredentials = Map<User, Set<KdmidCredentials>>
    type CityCredentials = Map<City, Set<KdmidCredentials>>

    type UserOrder =
        { User: User
          CityCredentials: CityCredentials }

    type CityOrder =
        { City: City
          UserCredentials: UserCredentials }

    type OrderResult =
        { Date: System.DateOnly
          Time: System.TimeOnly
          Description: string }

    type CityOrderResult = Map<User, Set<OrderResult>>
    type UserOrderResult = Map<City, Set<OrderResult>>

module Persistence =

    type User = { Id: string; Name: string }

    type KdmidCredentials = { Id: string; Cd: string; Ems: string }

    type UserKdmidOrders =
        { User: User
          Orders: Map<string, Set<KdmidCredentials>> }

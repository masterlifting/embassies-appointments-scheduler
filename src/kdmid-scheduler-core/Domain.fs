module KdmidScheduler.Domain

type KdmidCredentialId = Id of string
type KdmidCredentialCd = Cd of string
type KdmidCredentialEms = Ems of string option

type KdmidCredentials =
    { Id: KdmidCredentialId
      Cd: KdmidCredentialCd
      Ems: KdmidCredentialEms }

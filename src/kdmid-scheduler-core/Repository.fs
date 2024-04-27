module internal KdmidScheduler.Repository

open Persistence.Core.Storage

module User =
    let add user storage =
        async {
            return
                match storage with
                | MemoryStorage storage -> MemoryRepository.User.add user storage
                | _ -> Error $"Not implemented for '{storage}'."
        }

    let get id storage =
        async {
            return
                match storage with
                | MemoryStorage storage -> MemoryRepository.User.get id storage
                | _ -> Error $"Not implemented for '{storage}'."
        }

module UserCredentials =
    let add city credentials storage =
        async {
            return
                match storage with
                | MemoryStorage storage -> MemoryRepository.UserCredentials.add city credentials storage
                | _ -> Error $"Not implemented for '{storage}'."
        }

    let get city storage =
        async {
            return
                match storage with
                | MemoryStorage storage -> MemoryRepository.UserCredentials.get city storage
                | _ -> Error $"Not implemented for '{storage}'."
        }

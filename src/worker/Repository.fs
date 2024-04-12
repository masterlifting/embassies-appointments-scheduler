module Repository

let getTask name =
    async { return Converter.getTask name }

let getTasks = Converter.getTasks

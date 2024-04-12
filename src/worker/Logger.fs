module Logger

let private logLevel = Configuration.getSection<string> "Logging:LogLevel:Default"
let private logger = Infrastructure.Logging.getConsoleLogger logLevel

Worker.Logger.on logger

let trace = logger.logTrace
let debug = logger.logDebug
let info = logger.logInfo
let warning = logger.logWarning
let error = logger.logError

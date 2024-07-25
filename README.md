<!-- @format -->

# Embassy Access (F#)

# !Development in progress!

## Overview

Embassy Access is a project designed to facilitate the process of finding and booking appointments at various embassies and consulates. This includes appointments for visas, passport issuance, insurance signing, and other consulate services.

## Project Structure

The project is structured into several components, including core functionality, web APIs, worker services, and submodules for infrastructure, persistence, and web functionalities.

### Solution Structure

- **Submodules**

  - `fsharp-infrastructure`: Provides foundational infrastructure services.
  - `fsharp-persistence`: Handles data persistence.
  - `fsharp-worker`: Manages background tasks and processing.
  - `fsharp-web`: Web-related functionalities.

- **Source**

  - `embassy-access-core`: Core logic and functionalities.
  - `embassy-access-webapi`: Web API for external interactions.
  - `embassy-access-worker`: Background worker services.

- **Tests**
  - `embassy-access-tests`: Tests for the functionalities.

## Getting Started

### Prerequisites

- .NET SDK
- F#
- Visual Studio or any other compatible IDE

### Cloning the Repository

```bash
git clone https://github.com/masterlifting/embassy-access --recurse-submodules
```

### Building the Solution

Open embassy-access.sln in Visual Studio and build the solution. Ensure all submodules are correctly initialized and updated.

### Running the Application

- Web API: Start the embassy-access-webapi project.
- Worker: Start the embassy-access-worker project.

### Contributing

Contributions are welcome! Please fork the repository and submit a pull request.

### License

This project is licensed under the MIT License.

### Contact

For any inquiries or issues, please contact me via telegram at @ponkorn71

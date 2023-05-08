module Backends.ECS where

import Prelude

import Data.Maybe (Maybe)

type ARN = String

data AppProtocol = HTTP | HTTP2 | GRPC

data Protocol = TCP | UDP

type PortMapping = {
    appProtocol :: Maybe AppProtocol,
    containerPort :: Int,
    containerPortRange :: Maybe String,
    hostPortRange :: Maybe String,
    hostPort :: Maybe Int,
    name :: Maybe String,
    protocol :: Protocol
}

type HealthCheck = {
    command :: String,
    interval :: Int,
    timeout :: Int,
    retries :: Int,
    startPeriod :: Maybe Int
}

type Environment = {
    
}

type ContainerDefintion = {
    name :: String,
    image :: String,
    memory :: Maybe Memory,
    memoryReservation :: Maybe Memory,
    portMappings :: Array PortMapping,
    healthCheck :: Maybe HealthCheck,
    environment :: Maybe Environment
}

type LaunchType = { ec2 :: Boolean, fargate :: Boolean, external :: Boolean }

data NetworkMode = None | Bridge | Awsvpc | Host

data OperatingSystem = Linux | WindowsServer2022Core | WindowsServer2022Full | WindowsServer2019Full | WindowsServer2019Core | WindowsServer2016Full | WindowsServer2004Core | WindowsServer20H2Core

data CPUArch = X86_64 | ARM64

data CPU = C_256 | C_512 | C_1024 | C_2048 | C_4096 | C_8192 | C_16384

data Memory = M_512 | M_1024 | M_2048 | M_3072 | M_4096 | M_5120 | M_6144 | M_7168 | M_8192 -- TODO more

type TaskDefinition = {
    family :: String,
    requiresCompatibilities :: LaunchType,
    taskRoleArn :: Maybe ARN,
    executionRoleArn :: Maybe ARN,
    networkMode :: NetworkMode,
    operatingSystemFamily :: OperatingSystem,
    cpuArchitecture :: CPUArch,
    cpu :: Maybe CPU,
    memory :: Maybe Memory,
    containerDefinitions :: Array ContainerDefintion
}
module Configuration where

import Network.AMQP

gccPath = "/usr/bin/gcc"
nsJailPath = "/home/maxmati/src/foss/nsjail/nsjail" --TODO: use path from zmora-distro

rabbitMQConnectionOpts = defaultConnectionOpts

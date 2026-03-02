// executable/commands/BuildCommand.h
#pragma once

namespace CLI {
class App;
}

namespace blaze::cli {
void registerBuildCommand(CLI::App &parent, bool &noColor, bool &verbose);
}

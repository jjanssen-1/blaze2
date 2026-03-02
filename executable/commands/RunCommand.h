// executable/commands/RunCommand.h
#pragma once

namespace CLI {
class App;
}

namespace blaze::cli {
void registerRunCommand(CLI::App &parent, bool &noColor, bool &verbose);
}

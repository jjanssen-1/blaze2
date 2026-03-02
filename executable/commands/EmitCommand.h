// executable/commands/EmitCommand.h
#pragma once

namespace CLI {
class App;
}

namespace blaze::cli {
void registerEmitCommand(CLI::App &parent, bool &noColor, bool &verbose);
}

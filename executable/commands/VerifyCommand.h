// executable/commands/VerifyCommand.h
#pragma once

namespace CLI {
class App;
}

namespace blaze::cli {
void registerVerifyCommand(CLI::App &parent, bool &noColor, bool &verbose);
}

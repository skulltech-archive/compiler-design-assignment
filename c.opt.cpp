#include <iostream>

#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"

using namespace std;

struct Hello : public llvm::FunctionPass {
    static char ID;
    Hello() : llvm::FunctionPass(ID) {}
    bool runOnFunction(llvm::Function &func) override {
        llvm::errs() << "Hello : ";
        llvm::errs().write_escaped(func.getName()) << '\n';
        return false;
    }
};

struct RemoveUnconditionalBranch : public llvm::FunctionPass {
    static char ID;
    RemoveUnconditionalBranch() : llvm::FunctionPass(ID) {}
    bool runOnFunction(llvm::Function &func) override {
        for (auto &block : func) {
            if (auto *binst =
                    llvm::dyn_cast<llvm::BranchInst>(block.getTerminator())) {
                if (binst->isUnconditional()) {
                    llvm::BasicBlock *succ = binst->getSuccessor(0);
                    if (succ->getUniquePredecessor()) {
                        binst->removeFromParent();
                        // succ->replaceAllUsesWith(block);
                        block.getInstList().splice(block.end(),
                                                   succ->getInstList());
                        succ->eraseFromParent();
                    }
                }
            }
        }
        return true;
    }
};

struct RemoveConditionalBranch : public llvm::FunctionPass {
    static char ID;
    RemoveConditionalBranch() : llvm::FunctionPass(ID) {}
    bool runOnFunction(llvm::Function &func) override {
        for (auto &block : func) {
            if (auto *binst =
                    llvm::dyn_cast<llvm::BranchInst>(block.getTerminator())) {
                if (binst->isConditional()) {
                    llvm::Value *cond = binst->getCondition();
                    llvm::BasicBlock *ifBlock = binst->getSuccessor(0);
                    llvm::BasicBlock *elseBlock = binst->getSuccessor(1);
                    if (auto *cb = llvm::dyn_cast<llvm::ConstantInt>(cond)) {
                        llvm::BasicBlock *succ;
                        if (cb->getZExtValue()) {
                            succ = ifBlock;
                            elseBlock->eraseFromParent();
                        } else {
                            succ = elseBlock;
                            ifBlock->eraseFromParent();
                        }
                        binst->removeFromParent();
                        // succ->replaceAllUsesWith(block);
                        block.getInstList().splice(block.end(),
                                                   succ->getInstList());
                        succ->eraseFromParent();
                    }
                }
            }
        }
        return true;
    }
};

char RemoveConditionalBranch::ID = 0;
char RemoveUnconditionalBranch::ID = 1;
static llvm::RegisterPass<RemoveConditionalBranch> deadcondbr(
    "deadcondbr", "remove dead conditional pass", true, false);
static llvm::RegisterPass<RemoveUnconditionalBranch> deadbr(
    "deadbr", "remove dead conditional pass", true, false);
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

struct FoldConstant : public llvm::FunctionPass {
    static char ID;
    FoldConstant() : llvm::FunctionPass(ID) {}
    bool runOnFunction(llvm::Function &func) override {
        bool changed = false;
        vector<llvm::Instruction *> instsToDelete;

        for (auto &block : func) {
            for (auto &inst : block) {
                if (inst.isBinaryOp()) {
                    llvm::Value *left = inst.getOperand(0);
                    llvm::Value *right = inst.getOperand(1);
                    int opcode = inst.getOpcode();

                    if (opcode == 13 || opcode == 15) {  // Add and Sub
                        if (llvm::ConstantInt *lc =
                                llvm::dyn_cast<llvm::ConstantInt>(left)) {
                            if (lc->isZero()) {
                                instsToDelete.push_back(&inst);
                                inst.replaceAllUsesWith(right);
                                changed = true;
                            }
                        }
                        if (llvm::ConstantInt *rc =
                                llvm::dyn_cast<llvm::ConstantInt>(right)) {
                            if (rc->isZero()) {
                                instsToDelete.push_back(&inst);
                                inst.replaceAllUsesWith(left);
                                changed = true;
                            }
                        }
                    }
                    if (opcode == 14 || opcode == 16) {  // FAdd and FSub
                        if (llvm::ConstantFP *lc =
                                llvm::dyn_cast<llvm::ConstantFP>(left)) {
                            if (lc->isZero()) {
                                instsToDelete.push_back(&inst);
                                inst.replaceAllUsesWith(right);
                                changed = true;
                            }
                        }
                        if (llvm::ConstantInt *rc =
                                llvm::dyn_cast<llvm::ConstantInt>(right)) {
                            if (rc->isZero() == 0) {
                                instsToDelete.push_back(&inst);
                                inst.replaceAllUsesWith(left);
                                changed = true;
                            }
                        }
                    }
                    if (opcode == 17) {  // Mul
                        if (llvm::ConstantInt *lc =
                                llvm::dyn_cast<llvm::ConstantInt>(left)) {
                            if (lc->isZero()) {
                                instsToDelete.push_back(&inst);
                                inst.replaceAllUsesWith(left);
                                changed = true;
                            }
                            if (lc->isOne()) {
                                instsToDelete.push_back(&inst);
                                inst.replaceAllUsesWith(right);
                                changed = true;
                            }
                        }
                        if (llvm::ConstantInt *rc =
                                llvm::dyn_cast<llvm::ConstantInt>(right)) {
                            if (rc->isZero()) {
                                instsToDelete.push_back(&inst);
                                inst.replaceAllUsesWith(right);
                                changed = true;
                            }
                            if (rc->isOne()) {
                                instsToDelete.push_back(&inst);
                                inst.replaceAllUsesWith(left);
                                changed = true;
                            }
                        }
                    }
                    if (opcode == 18) {  // FMul
                        if (llvm::ConstantFP *lc =
                                llvm::dyn_cast<llvm::ConstantFP>(left)) {
                            if (lc->isZero()) {
                                instsToDelete.push_back(&inst);
                                inst.replaceAllUsesWith(left);
                                changed = true;
                            }
                            if (lc->isExactlyValue(1)) {
                                instsToDelete.push_back(&inst);
                                inst.replaceAllUsesWith(right);
                                changed = true;
                            }
                        }
                        if (llvm::ConstantFP *rc =
                                llvm::dyn_cast<llvm::ConstantFP>(right)) {
                            if (rc->isZero()) {
                                instsToDelete.push_back(&inst);
                                inst.replaceAllUsesWith(right);
                                changed = true;
                            }
                            if (rc->isExactlyValue(1)) {
                                instsToDelete.push_back(&inst);
                                inst.replaceAllUsesWith(left);
                                changed = true;
                            }
                        }
                    }
                    if (opcode == 20) {  // SDiv
                        if (llvm::ConstantInt *lc =
                                llvm::dyn_cast<llvm::ConstantInt>(left)) {
                            if (lc->isZero()) {
                                instsToDelete.push_back(&inst);
                                inst.replaceAllUsesWith(left);
                                changed = true;
                            }
                        }
                        if (llvm::ConstantInt *rc =
                                llvm::dyn_cast<llvm::ConstantInt>(right)) {
                            if (rc->isOne()) {
                                instsToDelete.push_back(&inst);
                                inst.replaceAllUsesWith(left);
                                changed = true;
                            }
                        }
                    }
                    if (opcode == 21) {  // FDiv
                        if (llvm::ConstantFP *lc =
                                llvm::dyn_cast<llvm::ConstantFP>(left)) {
                            if (lc->isZero()) {
                                instsToDelete.push_back(&inst);
                                inst.replaceAllUsesWith(left);
                                changed = true;
                            }
                        }
                        if (llvm::ConstantFP *rc =
                                llvm::dyn_cast<llvm::ConstantFP>(right)) {
                            if (rc->isExactlyValue(1)) {
                                instsToDelete.push_back(&inst);
                                inst.replaceAllUsesWith(left);
                                changed = true;
                            }
                        }
                    }
                    if (opcode == 28) {  // And
                        if (llvm::ConstantInt *lc =
                                llvm::dyn_cast<llvm::ConstantInt>(left)) {
                            if (lc->isZero()) {
                                instsToDelete.push_back(&inst);
                                inst.replaceAllUsesWith(left);
                                changed = true;
                            }
                        }
                        if (llvm::ConstantInt *rc =
                                llvm::dyn_cast<llvm::ConstantInt>(right)) {
                            if (rc->isZero()) {
                                instsToDelete.push_back(&inst);
                                inst.replaceAllUsesWith(left);
                                changed = true;
                            }
                        }
                    }
                    if (opcode == 29) {  // Or
                        if (llvm::ConstantInt *lc =
                                llvm::dyn_cast<llvm::ConstantInt>(left)) {
                            if (lc->isOne()) {
                                instsToDelete.push_back(&inst);
                                inst.replaceAllUsesWith(left);
                                changed = true;
                            }
                        }
                        if (llvm::ConstantInt *rc =
                                llvm::dyn_cast<llvm::ConstantInt>(right)) {
                            if (rc->isOne()) {
                                instsToDelete.push_back(&inst);
                                inst.replaceAllUsesWith(right);
                                changed = true;
                            }
                        }
                    }
                }
            }
        }
        for (auto inst : instsToDelete) {
            inst->eraseFromParent();
        }
        return changed;
    }
};

char RemoveConditionalBranch::ID = 0;
char RemoveUnconditionalBranch::ID = 1;
char FoldConstant::ID = 2;
static llvm::RegisterPass<RemoveConditionalBranch> deadcondbr(
    "deadcondbr", "remove dead conditional pass", true, false);
static llvm::RegisterPass<RemoveUnconditionalBranch> deadbr(
    "deadbr", "remove dead conditional pass", true, false);
static llvm::RegisterPass<FoldConstant> foldconst("foldconst",
                                                  "constant folding", true,
                                                  false);
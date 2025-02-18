#include "BasicBlock.hpp"
#include "Constant.hpp"
#include "Function.hpp"
#include "GlobalVariable.hpp"
#include "Instruction.hpp"
#include "LICM.hpp"
#include "PassManager.hpp"
#include <cstddef>
#include <memory>
#include <vector>

/**
 * @brief 循环不变式外提Pass的主入口函数
 * 
 */
void LoopInvariantCodeMotion::run() {

    loop_detection_ = std::make_unique<LoopDetection>(m_);
    loop_detection_->run();
    func_info_ = std::make_unique<FuncInfo>(m_);
    func_info_->run();
    for (auto &loop : loop_detection_->get_loops()) {
        is_loop_done_[loop] = false;
    }

    for (auto &loop : loop_detection_->get_loops()) {
        traverse_loop(loop);
    }
}

/**
 * @brief 遍历循环及其子循环
 * @param loop 当前要处理的循环
 * 
 */
void LoopInvariantCodeMotion::traverse_loop(std::shared_ptr<Loop> loop) {
    if (is_loop_done_[loop]) {
        return;
    }
    is_loop_done_[loop] = true;
    for (auto &sub_loop : loop->get_sub_loops()) {
        traverse_loop(sub_loop);
    }
    run_on_loop(loop);
}

// TODO: 实现collect_loop_info函数
// 1. 遍历当前循环及其子循环的所有指令
// 2. 收集所有指令到loop_instructions中
// 3. 检查store指令是否修改了全局变量，如果是则添加到updated_global中
// 4. 检查是否包含非纯函数调用，如果有则设置contains_impure_call为true
void LoopInvariantCodeMotion::collect_loop_info(
    std::shared_ptr<Loop> loop,
    std::set<Value *> &loop_instructions,
    std::set<Value *> &updated_global,
    bool &contains_impure_call) {
    
    // 当前循环所有指令
    for (auto bb : loop->get_blocks()) {
        for (auto &inst : bb->get_instructions()) {
            loop_instructions.insert(&inst);
            // 检测是否修改了全局变量
            if (auto store = dynamic_cast<StoreInst *>(&inst)) {
                auto ptr = store->get_operand(1);
                if (dynamic_cast<GlobalVariable *>(ptr)) {
                    updated_global.insert(ptr);
                }
            }
            // 检测非纯函数
            else if (auto call = dynamic_cast<CallInst *>(&inst)) {
                if (!func_info_->is_pure_function(call->get_function())) {
                    contains_impure_call = true;
                }
            }
        }
    }
    // 递归遍历子循环
    for (auto &sub_loop : loop->get_sub_loops()) {
        collect_loop_info(sub_loop, loop_instructions, updated_global,
                          contains_impure_call);
    }
}

/**
 * @brief 对单个循环执行不变式外提优化
 * @param loop 要优化的循环
 * 
 */
void LoopInvariantCodeMotion::run_on_loop(std::shared_ptr<Loop> loop) {
    std::set<Value *> loop_instructions;
    std::set<Value *> updated_global;
    bool contains_impure_call = false;
    collect_loop_info(loop, loop_instructions, updated_global, contains_impure_call);

    std::vector<Value *> loop_invariant;

    // 识别循环不变式指令
    bool changed;
    do {
        changed = false;
        for (auto inst : loop_instructions) {
            // 跳过已确定不变的指令
            if (std::find(loop_invariant.begin(), loop_invariant.end(), inst) != loop_invariant.end())
                continue;

            auto now_inst = dynamic_cast<Instruction *>(inst);

            // 跳过store、ret、br、phi、非纯函数调用等
            if (now_inst->is_store() || now_inst->is_ret() || now_inst->is_br() || now_inst->is_phi() ||
                (now_inst->is_call() && !func_info_->is_pure_function((Function *)now_inst->get_operand(0))))
                continue;

            // 特殊处理全局变量的load指令
            if (now_inst->is_load() &&
                (dynamic_cast<GlobalVariable *>(now_inst->get_operand(0)) && 
                dynamic_cast<GetElementPtrInst *>(now_inst->get_operand(0))))
                continue;

            // 检查所有操作数是否都是循环不变的
            bool operands_are_invariant = true;
            for (auto op : now_inst->get_operands()) {
                if (std::find(loop_invariant.begin(), loop_invariant.end(), op) == loop_invariant.end()) {
                    operands_are_invariant = false;
                    break;
                }
            }

            if (operands_are_invariant) {
                loop_invariant.push_back(now_inst);
                changed = true;
            }
        }
    } while (changed);

    if (loop->get_preheader() == nullptr) {
        loop->set_preheader(BasicBlock::create(m_, "", loop->get_header()->get_parent()));
    }

    if (loop_invariant.empty())
        return;

    auto preheader = loop->get_preheader();

    // 更新 phi 指令
    for (auto &phi_inst_ : loop->get_header()->get_instructions()) {
        if (phi_inst_.get_instr_type() != Instruction::phi)
            break;

        auto phi = &phi_inst_;
        preheader->add_instr_begin(phi);
        loop->get_header()->erase_instr(phi);
    }

    // 用跳转指令重构控制流图
    std::vector<BasicBlock *> pred_to_remove;
    for (auto &pred : loop->get_header()->get_pre_basic_blocks()) {
        auto latches = loop->get_latches();
        if (std::find(latches.begin(), latches.end(), pred) == latches.end()) { // 不是latches
            pred->remove_succ_basic_block(loop->get_header());
            pred->add_succ_basic_block(preheader);
        }
    }

    for (auto &pred : pred_to_remove) {
        loop->get_header()->remove_pre_basic_block(pred);
    }

    // 外提循环不变指令
    for (auto inst : loop_invariant) {
        preheader->add_instruction((Instruction *)inst);
        for (auto bb : loop->get_blocks()) {
            auto &instlist = bb->get_instructions();
            for (auto &now_inst : instlist) {
                if (&now_inst == (Instruction *)inst) {
                    bb->erase_instr((Instruction *)inst);
                }
            }
        }
    }

    // 插入 preheader 的跳转指令到 header
    BranchInst::create_br(loop->get_header(), preheader);

    // 将 preheader 插入父循环
    if (loop->get_parent() != nullptr) {
        loop->get_parent()->add_block(preheader);
    }
}

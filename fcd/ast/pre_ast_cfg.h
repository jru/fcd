//
// pre_ast_cfg.h
// Copyright (C) 2015 Félix Cloutier.
// All Rights Reserved.
//
// This file is distributed under the University of Illinois Open Source
// license. See LICENSE.md for details.
//

#ifndef pre_ast_cfg_h
#define pre_ast_cfg_h

#include "not_null.h"

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/Function.h>
#include <llvm/Analysis/DominanceFrontier.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Dominators.h>
#include <llvm/Support/GenericDomTree.h>
#include <llvm/Support/GenericDomTreeConstruction.h>

#include <deque>
#include <iterator>
#include <unordered_map>

class AstContext;
class Expression;
struct PreAstBasicBlock;
class PreAstContext;

struct PreAstBasicBlockEdge
{
	NOT_NULL(PreAstBasicBlock) from;
	NOT_NULL(PreAstBasicBlock) to;
	NOT_NULL(Expression) edgeCondition;
	
	PreAstBasicBlockEdge(PreAstBasicBlock& from, PreAstBasicBlock& to, Expression& edgeCondition)
	: from(&from), to(&to), edgeCondition(&edgeCondition)
	{
	}
	
	void setTo(PreAstBasicBlock& newTo);
};

struct PreAstBasicBlock
{
	using Pred = llvm::SmallVector<NOT_NULL(PreAstBasicBlockEdge), 8>;
	using Succ = llvm::SmallVector<NOT_NULL(PreAstBasicBlockEdge), 2>;

	Pred predecessors;
	Succ successors;
	
	llvm::BasicBlock* block;
	StatementReference blockStatement;
	
	PreAstBasicBlock(PreAstContext* parent);
	PreAstBasicBlock(const PreAstBasicBlock&) = delete;
	PreAstBasicBlock(PreAstBasicBlock&&);
	
	PreAstBasicBlock& operator=(PreAstBasicBlock&&);
	
	void swap(PreAstBasicBlock& block);
	void printAsOperand(llvm::raw_ostream& os, bool printType);

	PreAstContext* getParent() const
	{
		return parent;
	}

private:
	PreAstContext* parent;
};

class PreAstContext
{
	AstContext& ctx;
	std::deque<PreAstBasicBlockEdge> edgeList;
	std::deque<PreAstBasicBlock> blockList;
	std::unordered_map<llvm::BasicBlock*, PreAstBasicBlock*> blockMapping;
	
public:
	typedef decltype(blockList)::iterator node_iterator;
	
	PreAstContext(AstContext& ctx);
	
	void generateBlocks(llvm::Function& fn);
	PreAstBasicBlock& createRedirectorBlock(llvm::ArrayRef<PreAstBasicBlockEdge*> redirectedEdgeList);

	AstContext& getContext()
	{
		return ctx;
	}
	
	PreAstBasicBlockEdge& createEdge(PreAstBasicBlock& from, PreAstBasicBlock& to, Expression& edgeCondition)
	{
		edgeList.emplace_back(from, to, edgeCondition);
		return edgeList.back();
	}
	
	PreAstBasicBlock& createBlock()
	{
		blockList.emplace_back(this);
		return blockList.back();
	}
	
	PreAstBasicBlock* getEntryBlock()
	{
		return &blockList.front();
	}
	
	node_iterator begin()
	{
		return blockList.begin();
	}
	
	node_iterator end()
	{
		return blockList.end();
	}
	
	size_t size() const
	{
		return blockList.size();
	}
	
	void view() const;
};

struct PreAstBasicBlockRegionTraits
{
	typedef PreAstContext FuncT;
	typedef PreAstBasicBlock BlockT;
	typedef llvm::DominatorTreeBase<PreAstBasicBlock, false> DomTreeT;
	typedef llvm::DomTreeNodeBase<PreAstBasicBlock> DomTreeNodeT;
	typedef llvm::ForwardDominanceFrontierBase<PreAstBasicBlock> DomFrontierT;
	typedef llvm::DominatorTreeBase<PreAstBasicBlock, true> PostDomTreeT;
};

template<typename Iterator, typename Transformer>
struct PreAstBasicBlockIterator : public std::iterator<std::bidirectional_iterator_tag,
													   PreAstBasicBlock*, int,
													   NOT_NULL(PreAstBasicBlock),
													   PreAstBasicBlock*>
{
	typename std::remove_reference<Iterator>::type base;
	Transformer transformer;
	
	PreAstBasicBlockIterator(Iterator base, Transformer&& transformer)
	: base(base), transformer(transformer)
	{
	}
	
	NOT_NULL(PreAstBasicBlock) operator*() const
	{
		return transformer(*base);
	}
	
	PreAstBasicBlockIterator& operator++()
	{
		++base;
		return *this;
	}

	PreAstBasicBlockIterator& operator--()
	{
		--base;
		return *this;
	}
	
	PreAstBasicBlockIterator operator++(int)
	{
		auto copy = *this;
		++*this;
		return copy;
	}

	PreAstBasicBlockIterator operator--(int)
	{
		auto copy = *this;
		--*this;
		return copy;
	}
	
	difference_type operator-(const PreAstBasicBlockIterator& that) const
	{
		return base - that.base;
	}
	
	bool operator==(const PreAstBasicBlockIterator& that) const
	{
		return base == that.base;
	}
	
	bool operator!=(const PreAstBasicBlockIterator& that) const
	{
		return !(base == that.base);
	}
};

namespace
{
	template<typename Iterator, typename Action>
	auto makePreAstBlockIterator(Iterator&& iter, Action&& action)
	{
		return PreAstBasicBlockIterator<Iterator, Action>(std::forward<Iterator>(iter), std::forward<Action>(action));
	}

	auto makeSuccessorIterator(PreAstBasicBlock::Succ::iterator iter)
	{
		return makePreAstBlockIterator(iter, [](PreAstBasicBlockEdge* edge) { return edge->to; });
	}

	auto makePredecessorIterator(PreAstBasicBlock::Pred::iterator iter)
	{
		return makePreAstBlockIterator(iter, [](PreAstBasicBlockEdge* edge) { return edge->from; });
	}
	
	auto makeNodeListIterator(decltype(std::declval<PreAstContext>().begin()) iter)
	{
		return makePreAstBlockIterator(iter, [](PreAstBasicBlock& block) { return &block; });
	}
}

template<>
struct llvm::GraphTraits<PreAstBasicBlock*>
{
	typedef PreAstBasicBlock* NodeRef;
	typedef decltype(makeSuccessorIterator(std::declval<PreAstBasicBlock::Succ::iterator>())) ChildIteratorType;
	
	static NodeRef getEntryNode(PreAstBasicBlock* block)
	{
		return block;
	}
	
	static ChildIteratorType child_begin(NodeRef node)
	{
		return makeSuccessorIterator(node->successors.begin());
	}
	
	static ChildIteratorType child_end(NodeRef node)
	{
		return makeSuccessorIterator(node->successors.end());
	}
};

template<>
struct llvm::GraphTraits<llvm::Inverse<PreAstBasicBlock*>>
{
	typedef PreAstBasicBlock* NodeRef;
	typedef decltype(makePredecessorIterator(std::declval<PreAstBasicBlock::Succ::iterator>())) ChildIteratorType;
	
	static NodeRef getEntryNode(PreAstBasicBlock* block)
	{
		return block;
	}
	
	static ChildIteratorType child_begin(NodeRef node)
	{
		return makePredecessorIterator(node->predecessors.begin());
	}
	
	static ChildIteratorType child_end(NodeRef node)
	{
		return makePredecessorIterator(node->predecessors.end());
	}
};

struct PreAstContextGraphTraits
{
	typedef decltype(makeNodeListIterator(std::declval<PreAstContext>().begin())) nodes_iterator;
	
	static nodes_iterator nodes_begin(PreAstContext* f)
	{
		return makeNodeListIterator(f->begin());
	}
	
	static nodes_iterator nodes_end(PreAstContext* f)
	{
		return makeNodeListIterator(f->end());
	}
	
	static size_t size(PreAstContext* f)
	{
		return f->size();
	}
	
	static PreAstBasicBlock* getEntryNode(PreAstContext* context)
	{
		return context->getEntryBlock();
	}
};

template<>
struct llvm::GraphTraits<PreAstContext*>
: public llvm::GraphTraits<PreAstBasicBlock*>, public PreAstContextGraphTraits
{
	using llvm::GraphTraits<PreAstBasicBlock*>::getEntryNode;
	using PreAstContextGraphTraits::getEntryNode;
};

template<>
struct llvm::GraphTraits<llvm::Inverse<PreAstContext*>>
: public llvm::GraphTraits<Inverse<PreAstBasicBlock*>>, public PreAstContextGraphTraits
{
	using llvm::GraphTraits<Inverse<PreAstBasicBlock*>>::getEntryNode;
	using PreAstContextGraphTraits::getEntryNode;
};

template<>
struct llvm::GraphTraits<PreAstBasicBlockRegionTraits::DomTreeNodeT*>
: public llvm::DomTreeGraphTraitsBase<PreAstBasicBlockRegionTraits::DomTreeNodeT, PreAstBasicBlockRegionTraits::DomTreeNodeT::iterator>
{
};

template<>
struct llvm::GraphTraits<const PreAstBasicBlockRegionTraits::DomTreeNodeT*>
: public llvm::DomTreeGraphTraitsBase<const PreAstBasicBlockRegionTraits::DomTreeNodeT, PreAstBasicBlockRegionTraits::DomTreeNodeT::const_iterator>
{
};

#endif /* pre_ast_cfg_hpp */

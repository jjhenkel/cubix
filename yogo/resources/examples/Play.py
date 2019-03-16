# def split_slash(node):
#     """Recursively split slash-delimited node, e.g., Soviet/Russian/German.
#     Modifies the given subtree
#     Note that split_slash does not split the given 'node', but returns the list
#     of nodes that should be the result of spliting node. The upper-level caller
#     must take care of adding these nodes to the parent and removing the original
#     node from the tree.
#     Arguments:
#     node -- root of the current subtree
#     Return:
#     List of nodes that are the results of spliting node, or empty list.
#     """
#     nodes_added = []
#     nodes_removed = []
#     for child in node.modifiers:
#         nodes = split_slash(child)
#         if nodes:
#             nodes_added.extend(nodes)
#             nodes_removed.append(child)
#     node.remove_modifiers(nodes_removed)
#     node.add_modifiers(nodes_added)

#     if '/' not in node.text:
#         return []

#     texts = node.text.split('/')

#     tokens = [first_sentence_span(doc).root for doc in map(nlp, texts)]
#     new_nodes = []
#     for token, text in zip(tokens, texts):
#         # Need to give the original text because spacy might alter the text in
#         # the way we don't want. For example 'cable-stayed/suspension' is split
#         # into 'cable-stayed' and 'suspension', but spacy further split
#         # 'cable-stayed' into ['cable', '-', 'stayed'], which we don't want.
#         # Note that the reason we still need to run spacy on texts here is that
#         # we hope it will recognize NORPs, like in 'Soviet/Russian/German'.
#         new_node = HeadModifierTree(token, node.relation, text=text)
#         new_node.add_modifiers(node.modifiers)
#         new_nodes.append(new_node)
#     return new_nodes

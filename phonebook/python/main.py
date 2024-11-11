from typing import Dict, List
from dataclasses import dataclass

@dataclass
class Node:
    children: Dict[str, "Node"] = None
    leafs: List[str] = None

    def __post_init__(self):
        self.children = {}
        self.leafs = []

class Phonebook:
    _root = Node()

    def search_contact(self, name: str) -> list[str]:
        """
        :returns: numbers associated with contacts starting on `name`.
        """
        node = self._root
        for c in name:
            node = node.children[c]

        leafs = []
        nodes = [node]
        while nodes:
            node = nodes.pop()
            leafs.append(node.leafs)
            nodes += node.children.values()

        return [x for xs in leafs for x in xs]

    def add_contact(self, name: str, number: str) -> bool:
        """
        Registers contact's `number` under given `name`.
        """
        node = self._root
        for c in name:
            if not c in node.children:
                node.children[c] = Node()
            node = node.children[c]

        node.leafs.append(number)

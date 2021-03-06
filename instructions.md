# Traduction de Kawa vers IMP, sur un cas concret

On considère la classe Kawa définie par le code

```java
class point {
  attribute int x;
  attribute int y;

  method void point_constructor(int a, int b) {
    this.x = a;
    this.y = b;
  }

  method int somme() {
    return this.x + this.y;
  }
}
```


# Représentations

Un objet de la classe point est représenté par un pointeur vers un
bloc à trois champs contenant, dans l'ordre :
1. un pointeur vers le descripteur *(descr)* de la classe **point** *(décalage 0)*
2. la valeur de **x** *(décalage 4)*
3. la valeur de **y** *(décalage 8)*

```
  0       4       8
  +-------+-------+-------+
  | descr |   x   |   y   |
  +-------+-------+-------+
```

Le descripteur de classe est représenté par un bloc à trois champs
contenant, dans l'ordre :
1. un pointeur nul, la classe n'ayant pas de parent *(décalage 0)*
2. un pointeur vers le code de **point_constructor** *(pcstr, décalage 4)*
3. un pointeur vers le code de **somme** *(décalage 8)*

```
  0       4       8
  +-------+-------+-------+
  |   0   | pcstr | somme |
  +-------+-------+-------+
```

On peut se donner en plus une variable globale **point_descr** contenant
un pointeur vers le descripteur de classe.


# Traduction d'une instruction d'affectation

On regarde l'instruction Kawa
``` java
  p.x = 3 * p.x;
```

Le fragment `p.x` désigne l'accès à un champ.
L'AST Kawa pour ce fragment est noté en caml :

``` ocaml
  Field(Get(Var "p"), "x") (* on le note 'ma' ensuite *)
```

Ce fragment désigne une adresse est peut être utilisé aussi bien en lecture qu'en écriture, avec `Get` et `Set`. L'AST de l'instruction `p.x = 3 * p.x; `

est donc :

``` ocaml
  Set(ma, Binop(Mul, Cst 3, Get ma))
```

Le fragment `p.x` désigne une adresse, obtenue en ajoutant à l'adresse `p`
le décalage correspondant à l'attribut `x`, c'est-à-dire 4. Le fragment est donc traduit en PIMP par :
```
  p + 4                          // syntaxe concrète
```
ou
``` ocaml
  Binop(Add, Var "p", Cst 4)   // syntaxe asbtraite, on le note [ma'] ensuite
```

Pour le reste de l'instruction, l'accès `Get` est traduit par une opération de lecture, et l'instruction `Set` par une opération d'écriture.

L'instruction [p.x = 3*p.x] est donc traduite en PIMP par :
```
  *(p+4) = 3 * *(p+4); // syntaxe concrète
```
ou
``` ocaml
  Write(
    ma',
    Binop(
      Mul,
      Cst 3, 
      Unop(Read, ma')
    )
  ) (** syntaxe abstraite *)
```


# Traduction d'un appel de méthode

On regarde l'expression Kawa
```
p.somme()
```

L'AST Kawa correspondant est :
``` ocaml
  MethCall(Get(Var "p"), "somme", [])
```

Il combine
- un accès à la valeur de la variable `p`,
- le nom de la méthode `somme`,
- une liste vide d'arguments `[]`.

L'appel de méthode est traduit en **PIMP** par un appel de fonction avec `p` pour unique argument.

La fonction est désignée par un pointeur vers son code. En
notant `f` ce pointeur de code on aurait donc en PIMP :
```
  *f(p)             // syntaxe concrète
```
ou

``` ocaml
Call(FPointer f, [Var "p"])   (** syntaxe abstraite *)
```

Reste à calculer l'adresse du code de la fonction, sachant que le pointeur
cherché est stocké dans le descripteur de classe avec un décalage de 8.
On récupère donc le pointeur ainsi :

- Adresse de l'objet: `p`
- Contenu du premier champ de l'objet `*p`
  - ce dernier est également l'adresse du descripteur de classe
- Accès au champ du descripteur de classe : `*p+8`
- Contenu du champ : `*(*p+8)`
  - ce dernier est le pointeur de code cherché

En syntaxe abstraite :
``` ocaml
  Unop(Read, Binop(Add, Cst 8, Unop(Read, Var "p")))
```

Appel complet en PIMP :
```
  *(*p+8)(p)      // syntaxe concrète
```
ou
``` ocaml
  Call(
    FPointer(
      Unop(
        Read,
        (* adr of function *)
        Binop(Add, Cst 8, Unop(Read, Var "p")) 
      )
    ), 
    [Var "p"]
  )
  // syntaxe abstraite
```

# Traduction de la création d'un nouvel objet

On regarde l'expression Kawa
```
new point(2, 1)
```

En syntaxe abstraite Kawa, on donne le nom de la classe est la liste des
arguments :
```ocaml
  New("point", [Cst 2; Cst 1])
```

Pour créer l'objet il faut d'abord créer le bloc lui-même, puis initialiser
son en-tête, et enfin appeler la méthode `point_constructor`.
Supposons que `x0` soit le nom d'une nouvelle variable locale créée pour
l'occasion. On aurait donc, en syntaxe concrète PIMP, les trois instructions
```
  x0 = alloc(12);
  *x0 = point_descr;
  point_constructor(x0, 2, 1);
```
suivies par la simple expression `x0`

En syntaxe abstraite on aurait la structure suivante :
```ocaml
  Seq([
        Set(Var "x0", Unop(Alloc, Cst 12));
        Write(Var "x0", Var "point_descr");
        Expr(Call(FName "point_constructor", [Var "x0"; Cst 2; Cst 1]))
      ],
      Var "x0")
```

# Création du descripteur de classe

Le descripteur de classe est créé avant l'exécution du code principal.
Il ne correspond à aucune expression ou instruction Kawa, et est
simplement déclenché par l'existence de la classe.

Pour créer le descripteur il faut créer le bloc lui-même, initialiser
la variable globale `point_descr` avec l'adresse de ce bloc, et placer
dans les champs 2 et 3 les adresses des méthodes. On aurait donc, en
syntaxe concrète PIMP :
```
  point_descr = alloc(12);
  *(point_descr+4) = &point_constructor;
  *(point_descr+8) = &somme;
```

En syntaxe abstraite cela donne la liste d'instructions suivante :
```ocaml
  [
    Set(Var "point_descr", Unop(Alloc, Cst 12));
    Write(Binop(Add, Var "point_descr", Cst 4), Addr "point_constructor");
    Write(Binop(Add, Var "point_descr", Cst 8), Addr "somme")
  ]
```


# Progression

Pour séparer les difficultés, vous pouvez d'abord **compléter** et **tester** un
compilateur pour une version réduite du langage Kawa, puis intégrer petit à
petit les éléments qui avaient été laissés de côté dans les premières
itérations.

On peut notamment dans un premier temps ignorer les types, interdire à deux
classes différentes d'introduire des attributs ou méthodes de même nom, et
interdire l'héritage. Puis, une fois que cette version réduite fonctionne, on
peut réintégrer ces différents aspects l'un après l'autre.


## 1. Version minimale
----------------------

On décrit ici la version réduite de Kawa qui ignore les **types et l'héritage**,
et impose une séparation des noms utilisés par chaque classe.
Notez que cette version est suffisante pour l'exemple de la classe `point`.

Si l'on impose que les noms d'attributs introduits par chacune des classes
forment des ensembles disjoints, alors chaque identifiant d'attribut n'est
utilisé qu'une fois. En conséquence :

- le décalage associé à un attribut peut être déduit directement du nom de
  cet attribut, sans besoin de connaître la classe de l'objet concerné

- on peut donc avoir à l'échelle du programme, une table unique associant
  chaque attribut à son décalage

On peut de même avoir une unique table pour les décalages associés aux noms
de méthodes. Du fait de cette table unique, au moment de compiler un accès
à un attribut `obj.x` on n'a aucun besoin de connaître la classe de `obj`.
On peut donc complètement ignorer les types dans cette version.


En revanche, pour écrire un programme Kawa correct dans cette version, il
faut s'assurer que toutes les méthodes de toutes les classes ont bien des
noms différents, y compris les constructeurs. On fixera la convention
suivante, appliquée dans l'exemple de la classe `point` : le constructeur
d'une classe `C` est une méthode nommée `C_constructor`.


En l'absence d'héritage, chaque classe possède exactement les attributs et
les méthodes déclarés dans sa définition. En conséquence :

- le nombre de champs du bloc mémoire représentant un objet `obj` d'une
  classe `C` est directement calculé comme
    1 + nombre d'attributs dans la définition de C
  (de même pour le descripteur de classe en fonction des méthodes)
    
- le décalage permettant l'accès à un attribut `x` est calculé directement
  en fonction de la position de `x` dans la liste énumérant les attributs
  de la classe
  (de même pour les décalages permettant d'accéder aux méthodes)
  
- un descripteur de classe ne contient que les méthodes déclarées dans la
  classe correspondante

Notez que vous pouvez traiter cette version minimale sans modifier la
définition de l'AST, il vous suffit d'ignorer le champ `parent` des
définitions de classes, ainsi que toutes les annotations de types.


Une fois cette version minimale complète, les deux prochaines étapes sont :
- inclure de l'héritage
- tenir compte des types et permettre le réemploi des noms

Ces deux extensions sont indépendantes et peuvent être traitées dans
n'importe quel ordre. Une fois qu'elles sont réalisées toutes les deux, de
nouvelles extensions deviennent possibles.


### 2A. Héritage
------------

On veut maintenant permettre l'utilisation de `extends` dans les programmes
Kawa, pour définir une classe fille héritant des attributs et méthodes d'une
classe mère, avec en outre la possibilité :
  1. d'ajouter de nouveaux attributs
  2. d'ajouter de nouvelles méthodes
  3. de redéfinir des méthodes de la classe mère

On peut à nouveau procéder par étapes, en complétant **1** avant de passer
à **2** puis en complétant **2** avant de passer à **3**.

Notez que tous ces ajouts donnent une certaine forme de partage de noms
d'attributs ou de méthodes entre plusieurs classes, mais sans affecter la
propriété que chaque nom d'attribut ou de méthode est associé de manière
unique à un décalage, à l'échelle du programme.


#### 2A.1 Extension des attributs
------------------------------

Par rapport à la version précédente, il faut tenir compte du nombre
d'attributs de la classe mère pour connaître le nombre total d'attributs
et les décalages correspondant aux nouveaux attributs.

```
  0       4                         ?
  +-------+-------------------------+--------------------------+
  | descr | ...attributs hérités... | ...nouveaux attributs... |
  +-------+-------------------------+--------------------------+
```

En revanche, en l'absence de nouvelles méthodes, on peut se passer d'un
nouveau descripteur de classe et simplement utiliser le descripteur de la
classe mère.

Notez qu'en pratique vous voudrez ajouter au moins une méthode de
construction. Cependant, dans les cas simples cette méthode est appelée
directement et on peut donc (dans un premier temps) s'épargner la
construction d'un nouveau descripteur.


#### 2A.2 Extension des méthodes
------------------------------

Similairement à la version précédente, il faut tenir compte du nombre de
méthodes de la classe mère pour connaître le nombre total de méthodes
et les décalages correspondant aux nouvelles méthodes.


Ici en plus, il faut prévoir la construction du nouveau descripteur,
reprenant les méthodes de la classe mère et y ajoutant les nouvelles
méthodes.


#### 2A.3 Redéfinition des méthodes
----------------------------------
Lorsqu'un classe fille `C'` redéfinit une méthode `m` déjà présente dans
la classe mère `C`, la nouvelle définition doit prendre la place de
l'ancienne dans le descripteur de `C'`. Il faut donc
  - ne pas affecter un nouveau décalage à `m` dans `C'`
  - dans le construction du descripteur de classe de `C'`, s'assurer qu'on
    utilise bien le pointeur correspondant à la version redéfinie de `m`


#### 2A.4 Bonus : super
---------------------
  
Une fois la redéfinition possible, vous pouvez souhaiter ajouter le mot-clé
`super`, qui permet l'accès à la version mère d'une méthode redéfinie par
une classe fille.


### 2B. Séparation des espaces de noms
----------------------------------

On veut que deux classes puissent définir des attributs ou méthodes de
mêmes noms, avec potentiellement des décalages différents. Cela revient
à créer une table des attributs et une table des méthodes pour chaque
classe, au lieu d'avoir des tables uniques à l'échelle du programme.

En revanche, on a maintenant besoin d'informations sur la classe à laquelle
appartient un objet pour résoudre l'accès à un attribut. Si on considère
un accès `obj.x`

Il faut
  - déterminer la classe `C` à laquelle appartient `obj`
  - puis aller consulter la table des attributs de la classe `C` pour
    connaître, dans cette situation, le décalage de l'attribut `x`

La même chose vaut pour les méthodes.

Pour déterminer la classe de `obj`, on a besoin de consulter les annotations
de types données par le programmeur. Dans le cas de base où l'objet `obj` est
directement donné par une variable, il suffit d'aller consulter la déclaration
de cette variable (attention cependant : il peut s'agit d'une variable locale,
d'un paramètre de méthode ou d'une variable globale). Le cas où `obj` est
`this` est direct également. Dans un cas plus général, `obj` peut être donné
par une expression quelconque, et il faut une fonction auxiliaire pour calculer
le type de cette expression.


#### 2B.(+) Bonus
------------
  
Ce calcul du type des expressions est aussi l'occasion de renvoyer des messages
d'erreur à l'utilisateur, si le programme Kawa donné en entrée est incohérent.
Exemple d'incohérence : accès à un attribut ou une méthode inexistante dans la
classe de l'objet considéré.


### 3. Bonus ! Bonus !! Bonus !!!
----------------------

Si vous avez complété **2A** et **2B**, de nombreuses autres extensions combinant
héritage et types s'ouvrent à vous. Par exemple :

- inclure une expression `obj instanceof C`, testant si l'objet `obj`
  appartient bien à une classe `C'` descendant de la classe `C`

- permettre le transtypage d'un objet `obj` de type apparent [C], vers
  un autre type `C'` (rappel : transtyper vers une classe mère ou ascendante
  est toujours possibles, transtyper vers une classe fille ou descendante
  nécessite un test à l'exécution, et transtyper vers une classe qui n'est
  ni ascendante ni descendante est impossible)

- permettre la définition de classes abstraites, contenant des méthodes
  abstraites (rappel : interdiction de faire `new` sur une classe abstraite,
  les classes filles doivent ultimement donner une définition à chaque
  méthode abstraite, en revanche le décalage correspondant à chaque méthode
  abstraite est fixé une fois pour toute dans la classe mère abstraite, et
  donc identique dans chaque classe fille ayant défini la méthode)
# h2o-erlang
Trabalho de Erlang para a cadeira de processamento paralelo

## Enunciado:

Kepler-20PB é um exoplaneta que orbita a Kepler-186. Trata-se um dos planetas de tamanho semelhante ao da Terra, descoberto na zona habitável de uma estrela. O Kepler-20PB possui um elemento misterioso que produz constantemente moléculas de dois tipos, hidrogênio e oxigênio, que quando combinadas geram uma partícula de água. Estas partículas, quando criadas, são instáveis e precisam de um tempo para adquirir energia suficiente para se combinar. Após um determinado tempo, as moléculas adquirem energia suficiente, o que permite sua combinação para gerar a partícula de água. Esta associação acontece sempre com duas moléculas de hidrogênio e uma molécula de oxigênio. Não são permitidas combinações diferentes desta, e para que aconteça as três moléculas devem estar em seu nível de energia mínimo ideal.

O problema acima apresentado foi adaptado da obra Andrews (1991).

Com base na descrição apresentada, desenvolva uma aplicação em Erlang que simule este fenômeno. Para esta tarefa devem ser respeitadas as elucidações apresentadas acima, bem como os seguintes requisitos:

    Cada molécula gerada, hidrogênio e oxigênio deve ser um processo em Erlang;
    O tempo para que cada molécula adquira energia suficiente deve variar entre 10s e 30s;
    A geração de moléculas deve ser constante e de forma aleatória com intervalo de tempo parametrizável;
    Cada processo deve ser identificado unicamente e apresentar uma mensagem quando criado informando esta identificação;
    A aplicação deve identificar as combinações realizadas, apresentando a identificação dos elementos combinados;

## Execução do código

Para executar o código, é necessário compilar ele com erlang. Em seguida, é só inicar através de "h2o:start()."
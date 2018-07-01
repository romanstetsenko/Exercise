module Model

type Word = Word of string
type Sentence = Sentence of Word list
type Text = InputText of Sentence list

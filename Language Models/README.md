# Finite State Transducers

<br> A language model is used to model the occurences of various sentences within a language.
It provides a definitive way of calculating the negative log likelihood for each input sentence.
The closer this value is to zero, the more probable the sentence is.
 </br>
<br> This code implements a bigram language model using the corpus provided in Corpus.txt. 
The following steps are carried out in sequence: </br>
<ol>
	<li> The corpus is loaded in a unix environment and normalization techniques are applied in order to make it 
		appropriate for the creation of the language model (unix_nitinnat.txt contains these unix commands).
		<ul>
			<li> Capitalizations are removed. </li>
			<li> Contractions such as i've, it's, etc are replaced with their corresponding expansions,
			such as "i have" and "it is". There is a huge problem of ambiguity in this situation, where in
		it's could mean "it has" or "it is" or "its" in the possessive case.
		Since I didn't have any POS-tags to work with, after checking the corpus it can be seen that "it has" usually
		follows with "been". Hence, the contractions were expanded accordingly.
		</li> 
			<li> Several other processing methods for punctuations were also applied. </li>
			<li> Finally, the @ symbol was used as a sentence boundary, by replacing periods, question marks,
			exclamation marks and double quotes in certain places. </li>
		</ul>
	</li>
	<li>
		The bigram counts and the unigram counts were then obtained and converted into Prolog-readable format.
		These counts will be useful for the next step, when we calculate log probabilities of sentences.
		</li>
		<li>
		The bigram counts and the unigram counts were then obtained and converted into Prolog-readable format.
		These counts will be useful for the next step, when we calculate log probabilities of sentences.
		</li>
		<li>
		Laplacian Smoothing and Add-alpha Smoothing was applied in order to account for non-existent ungirams and bigrams.
		</li>

</ol>

<br>  Test the probability of an input sentence. 
Open Prolog and run the following commands.
<ul>
<li>  [lm_nitinnat_all]. </li>
<li>  calc_prob([@,book, the, that,he,wanted,fell,my,on,feet, @],X).</li>
</ul>

</br>

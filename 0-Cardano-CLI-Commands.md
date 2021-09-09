# Queries

<pre><code>./cardano-cli query utxo --address $ADDRESS --testnet-magic XX</code></pre>

<pre><code>./cardano-cli query tip --testnet-magic XX</code></pre>

<hr />

# Addresses (BYRON)

<pre><code>./cardano-cli address key-gen --byron-key \
--verification-key-file bpayment.vkey \
--signing-key-file bpayment.skey

./cardano-cli address build \
--payment-verification-key-file ./bpayment.vkey \
--out-file bpayment.addr \
--testnet-magic XX
</code></pre>

<hr />

# Addresses (SHELLY)

<pre><code>./cardano-cli address key-gen \
--verification-key-file payment.vkey \
--signing-key-file payment.skey
</pre></code>
<pre><code>./cardano-cli stake-address key-gen \
--verification-key-file stake.vkey \
--signing-key-file stake.skey
</pre></code>
<pre><code>./cardano-cli address build \
--payment-verification-key-file ./payment.vkey \
--stake-verification-key-file ./stake.vkey \
--out-file payment.addr \
--testnet-magic XX
</pre></code>

<hr />

# Transactions

<pre><code>./cardano-cli query protocol-parameters \
--testnet-magic XX \
--out-file protocol.json
</pre></code>
<pre><code>./cardano-cli transaction build-raw \
--tx-in 86594137f6c58ca2cf94cd05bea3259b20b3f2fd07edb4e14a402ec0e5d72f5a#0 \
--tx-out $ADDRESSTWO+1000000000000 \
--tx-out $ADDRESS+0 \
--invalid-hereafter 0 \
--fee 0 \
--out-file tx.draft
</pre></code>
<pre><code>./cardano-cli transaction calculate-min-fee \
--tx-body-file tx.draft \
--tx-in-count 1 \
--tx-out-count 2 \
--witness-count 1 \
--byron-witness-count 0 \
--testnet-magic XX \
--protocol-params-file protocol.json
</pre></code>

**Remember the bloody fees!**

<big>*Calculate: 1000000000000 - 100000000000 - 176897*</big>

</pre></code>
<pre><code>./cardano-cli transaction build-raw \
--tx-in 86594137f6c58ca2cf94cd05bea3259b20b3f2fd07edb4e14a402ec0e5d72f5a#0 \
--tx-out $ADDRESSTWO+100000000000 \
--tx-out $ADDRESS+899999823103 \
--invalid-hereafter 394549 \
--fee 176897 \
--out-file tx.raw
</pre></code>
<pre><code>./cardano-cli transaction sign \
--tx-body-file tx.raw \
--signing-key-file payment.skey \
--testnet-magic XX \
--out-file tx.signed
</pre></code>
<pre><code>./cardano-cli transaction submit \
--tx-file tx.signed \
--testnet-magic XX
</pre></code>

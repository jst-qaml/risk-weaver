# RiskWeaver

A Domain Specific Language for the Specification of Risk-oriented Object Detection Requirements. Featuring:

* Relation-based Object Analysis
* Risk Factor Assessment
* Error Type Categorization
* Comprehensive Metrics
* Environment and Object Modeling
* Performance Evaluation Tools
* Data filtering
* Visualization

The formats of input/output data are based on [COCO](https://cocodataset.org/#format-data) and [COCO Results](https://cocodataset.org/#format-results).

## Getting Started

## Installation

1. Git clone the repository.
```bash
git clone https://github.com/jst-qaml/risk-weaver.git
```
2. Install the required packages using the following command:
```bash
#Install ghcup
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
#Install ghc and cabal
ghcup install ghc 9.6.3
ghcup set ghc 9.6.3
ghcup install cabal 3.10.2.0
ghcup set cabal 3.10.2.0
# Build the project
cabal update
cabal build all
cabal install
```
Then it installs risk-weaver-exe in the .cabal/bin directory.
When you update risk definision, you need to run the following command to update the risk-weaver-exe.

3. Generate a template code for the RiskWeaver DSL.
```bash
risk-weaver-exe generate-template > custom-risk-weaver.hs
```
4. Update risk definition(riskForGroundTruth and riskForDetection) in the generated template file.
5. Run the RiskWeaver code to generate the output.
```bash
# Build the updated code.
cabal exec ghc custom-risk-weaver.hs
```
Then use "custom-risk-weaver" command instead of risk-weaver-exe.

## Usage

### Analyzing the Risk of Object Detection

1. Generate a coco result output for an analized model and test dataset.
At first you need to add the following code to generate the output.
```python
```

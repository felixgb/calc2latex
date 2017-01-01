<template>
  <div id="app">
    <calc></calc>
  </div>
</template>

<script>
import Calc from './components/Calc'

export default {
  name: 'app',
  components: {
    Calc
  }
}
</script>

<style>
* {
  padding: 0px;
  margin: 0px;
}

span, p {
  font-family: Times, serif
}

#all {
  width: 1000px;
  display: flex;
  align-items: flex-start;
  flex-direction: column;
}

h1 {
  font-family: 'Avenir', Helvetica, Arial, sans-serif;
  padding: 25px;
  font-size: 50px;
  font-weight: heavier;
}

header > span {
  font-size: large;
  font-weight: 100;
}

header {
  width: 1000px;
  border-bottom: 1px solid black;
}

</style>

<template>
  <div class="hello">
    <button v-on:click="getImgData">Derivate</button>
    <input v-model="code">
    <br>
    <img v-bind:src="thing">
  </div>
</template>

<script>
export default {
  name: 'hello',
  data () {
    return {
      code: '',
      thing: ''
    }
  },
  methods: {
    getImgData: function () {
      this.$http.get('http://localhost:3000/' + btoa(this.code)).then((response) => {
        this.thing = 'data:image/png;base64,' + response.body
      }, (response) => {
        console.log('bad')
      })
    }
  }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
h1, h2 {
  font-weight: normal;
}

ul {
  list-style-type: none;
  padding: 0;
}

li {
  display: inline-block;
  margin: 0 10px;
}

a {
  color: #42b983;
}
</style>
